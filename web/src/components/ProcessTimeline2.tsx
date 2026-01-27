import { AppContext, type IAppContext } from "app/AppContext";
import React, { type FC, useContext, useEffect, useState } from "react";
import type { ProcessTimelines, TimelinePoint } from "services/gen/types";
import { api, type ProcessData } from "services/HaskellApiService";
import "components/ProcessTimeline2.scss";
import { COMPONENT_COLORS, Color, fadeColor } from "../utils/color";
import { JsonView } from "./JsonView";
import { ArrowLabel, ArrowPath, type ArrowProps } from "./utils/ArrowWithLabel";

enum OutputPosition {
  Left,
  Right,
  Both,
  None,
}

interface Instruction {
  pID: number;
  label: string;
  startTime: number;
  endTime: number;
  info: string;
  inputs: Set<string>;
  outputs: Set<string>;
  receiveInputsFromPIDs: Map<string, number>;
  sendsOutputsToPIDs: Map<string, number>;
  outputPosition: OutputPosition;
}

interface Function {
  label: string;
  component: string;
  pID: number;
  startTime: number;
  endTime: number;
  instructions: Instruction[];
  lowerPIDs: Set<number>;
  column: number;
  width: number;
  isMemoryInit: boolean;
}

const MIN_COLUMN_WIDTH = 50;
const ROW_HEIGHT = 70;
const TEXT_PADDING = 40;
const COLUMN_MARGIN = 20;
const HEADER_PADDING = 1.5; // padding in rem, converted to pixels
const BASE_FONT_SIZE = 16; // pixels
const MIN_FUNCTION_GAP = 0.8; // minimum time-unit gap between functions in same column

interface InstructionPosition {
  instructionId: number;
  x: number;
  y: number;
  width: number;
  height: number;
  color: string;
}

interface DataFlowConnection {
  sourceId: number;
  targetId: number;
  variableName: string;
}

const getArrowProps = (
  source: InstructionPosition,
  target: InstructionPosition,
  topPadding: number,
  label: string,
) => {
  let arrowSourceX: number;
  let arrowTargetX: number;

  if (source.x > target.x) {
    arrowSourceX = source.x;
    arrowTargetX = target.x + target.width;
  } else if (source.x === target.x) {
    arrowSourceX = source.x + source.width;
    arrowTargetX = target.x + target.width;
  } else {
    arrowSourceX = source.x + source.width;
    arrowTargetX = target.x;
  }
  const result: ArrowProps = {
    sourceX: arrowSourceX,
    sourceY: topPadding + source.y,
    sourceHeight: source.height,
    targetX: arrowTargetX,
    targetY: topPadding + target.y,
    targetHeight: target.height,
    label: label,
    color: "#404040",
  };
  return result;
};

export const ProcessTimelines2: FC = () => {
  const { selectedSid } = useContext(AppContext) as IAppContext;

  const [functions, setFunctions] = useState<Function[]>([]);
  const [timelineConfig, setTimelineConfig] = useState({
    minTime: 0,
    maxTime: 10,
  });
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [containerHeight, setContainerHeight] = useState(1000);
  const [columnWidths, setColumnWidths] = useState<Map<number, number>>(
    new Map(),
  );
  const [dataFlowConnections, setDataFlowConnections] = useState<
    DataFlowConnection[]
  >([]);
  const [instructionPositions, setInstructionPositions] = useState<
    Map<number, InstructionPosition>
  >(new Map());
  const [headerHeights, setHeaderHeights] = useState<Map<number, number>>(
    new Map(),
  );
  const containerRef = React.useRef<HTMLDivElement>(null);
  const [
    mostLeftFreeSpacesInColumnsPerRows,
    setMostLeftFreeSpacesInColumnsPerRows,
  ] = useState<Map<number, Map<number, number>>>(new Map());
  const selectedColorsRef = React.useRef<Map<string, string>>(new Map());
  const occupiedColorsRef = React.useRef<Set<string>>(new Set());
  const [topPadding, setTopPadding] = useState(0);
  const measurementTimeoutRef = React.useRef<NodeJS.Timeout | null>(null);

  const getComponentColor = React.useCallback((component: string): Color => {
    const preselectedColor = selectedColorsRef.current.get(component);
    if (preselectedColor) return COMPONENT_COLORS[preselectedColor];

    const hashCode = component
      .split("[")[0]
      .split("")
      .reduce((hash, char) => ((hash << 5) - hash + char.charCodeAt(0)) | 0, 0);

    const colorKeys = Object.keys(COMPONENT_COLORS).filter(
      (k) =>
        k !== "default" &&
        (!occupiedColorsRef.current.has(k) ||
          occupiedColorsRef.current.size ===
            Object.keys(COMPONENT_COLORS).length),
    );

    const selectedColor = colorKeys[Math.abs(hashCode) % colorKeys.length];
    selectedColorsRef.current.set(component, selectedColor);
    occupiedColorsRef.current.add(selectedColor);
    return COMPONENT_COLORS[selectedColor];
  }, []);

  const mapsEqual = React.useCallback(
    (map1: Map<number, number>, map2: Map<number, number>): boolean => {
      if (map1.size !== map2.size) return false;
      for (const [key, value] of map1) {
        if (map2.get(key) !== value) return false;
      }
      return true;
    },
    [],
  );

  // calculate header heights based on text wrapping within function width
  const calculateHeaderHeights = React.useCallback(() => {
    const container = containerRef.current;
    if (!container) return;

    const heightsMap = new Map<number, number>();

    functions.forEach((func) => {
      const headerId = `func-header-${func.pID}`;
      const headerElement = container.querySelector(
        `[data-header-id="${headerId}"]`,
      ) as HTMLElement;

      if (headerElement) {
        const measuredHeight = headerElement.offsetHeight;
        heightsMap.set(func.pID, Math.max(measuredHeight, 50)); // Minimum height of 50px
      } else {
        // Fallback to default if element not found
        heightsMap.set(func.pID, ROW_HEIGHT);
      }
    });

    // only update state if heights actually changed
    setHeaderHeights((prevHeights) => {
      if (mapsEqual(prevHeights, heightsMap)) {
        return prevHeights;
      }
      return heightsMap;
    });
  }, [functions, mapsEqual]);

  // helper function to compare InstructionPosition maps for equality
  const instructionPositionsEqual = React.useCallback(
    (
      map1: Map<number, InstructionPosition>,
      map2: Map<number, InstructionPosition>,
    ): boolean => {
      if (map1.size !== map2.size) return false;
      for (const [key, pos1] of map1) {
        const pos2 = map2.get(key);
        if (
          !pos2 ||
          pos1.x !== pos2.x ||
          pos1.y !== pos2.y ||
          pos1.width !== pos2.width ||
          pos1.height !== pos2.height ||
          pos1.color !== pos2.color
        ) {
          return false;
        }
      }
      return true;
    },
    [],
  );

  // calculate actual instruction positions from DOM after render
  const calculateInstructionPositions = React.useCallback(() => {
    const container = containerRef.current;
    if (!container) return;

    const positionsMap = new Map<number, InstructionPosition>();
    const containerRect = container.getBoundingClientRect();
    const paddingTop =
      container.offsetHeight > 0
        ? parseFloat(window.getComputedStyle(container).paddingTop)
        : 0;

    functions.forEach((func) => {
      func.instructions.forEach((instr) => {
        const elemId = `instr-${instr.pID}`;
        const element = container.querySelector(
          `[data-instruction-id="${elemId}"]`,
        );

        if (element) {
          const rect = element.getBoundingClientRect();
          const relativeX =
            rect.left - containerRect.left + container.scrollLeft;
          const relativeY =
            rect.top - containerRect.top + container.scrollTop - paddingTop;

          positionsMap.set(instr.pID, {
            instructionId: instr.pID,
            x: relativeX,
            y: relativeY,
            width: rect.width,
            height: rect.height,
            color: getComponentColor(func.component).toHexString(),
          });
        }
      });
    });

    // only update state if positions actually changed
    setInstructionPositions((prevPositions) => {
      if (instructionPositionsEqual(prevPositions, positionsMap)) {
        return prevPositions;
      }
      return positionsMap;
    });
  }, [functions, getComponentColor, instructionPositionsEqual]);

  // recalculate header heights and top padding on render
  useEffect(() => {
    if (!loading && functions.length > 0) {
      // Clear any pending measurement
      if (measurementTimeoutRef.current) {
        clearTimeout(measurementTimeoutRef.current);
      }

      // schedule header height measurement after render completes
      measurementTimeoutRef.current = setTimeout(() => {
        calculateHeaderHeights();

        // calculate top padding to account for headers extending above minTime
        if (timelineConfig.minTime === 0) {
          const functionsAtMinTime = functions.filter(
            (f) => f.startTime === timelineConfig.minTime,
          );
          const maxHeaderHeight = Math.max(
            ...functionsAtMinTime.map(
              (f) => headerHeights.get(f.pID) || ROW_HEIGHT,
            ),
            0,
          );
          setTopPadding(maxHeaderHeight);
        }
      }, 100);

      const handleResize = () => {
        if (measurementTimeoutRef.current) {
          clearTimeout(measurementTimeoutRef.current);
        }
        measurementTimeoutRef.current = setTimeout(() => {
          calculateHeaderHeights();
        }, 100);
      };

      window.addEventListener("resize", handleResize);
      return () => {
        if (measurementTimeoutRef.current) {
          clearTimeout(measurementTimeoutRef.current);
        }
        window.removeEventListener("resize", handleResize);
      };
    }
  }, [
    functions,
    loading,
    calculateHeaderHeights,
    timelineConfig,
    headerHeights,
  ]);

  // recalculate instruction positions when header heights change
  useEffect(() => {
    if (!loading && functions.length > 0 && headerHeights.size > 0) {
      const positionTimer = setTimeout(() => {
        calculateInstructionPositions();
      }, 100);

      return () => clearTimeout(positionTimer);
    }
  }, [functions, loading, calculateInstructionPositions, headerHeights]);

  // estimate header height based on text wrapping within expected function width
  const estimateHeaderHeight = React.useCallback(
    (headerText: string, functionWidth: number): number => {
      // account for padding and margins in the header
      const availableWidth =
        functionWidth - HEADER_PADDING * BASE_FONT_SIZE * 2; // left and right padding

      // estimate characters per line based on monospace font
      // average character width in monospace is approximately 0.6em = 9.6px at 16px base
      const charWidth = 9.6;
      const charsPerLine = Math.max(1, Math.floor(availableWidth / charWidth));

      // split header into lines and count how many lines needed
      let totalLines = 1;
      const lines = headerText.split("\n");

      for (const line of lines) {
        const wrappedLines = Math.ceil(line.length / charsPerLine);
        totalLines += wrappedLines - 1;
      }

      const lineHeight = 1.1 * BASE_FONT_SIZE;
      const padding = HEADER_PADDING * BASE_FONT_SIZE * 2; // top and bottom
      const estimatedHeight = totalLines * lineHeight + padding;

      return Math.max(50, estimatedHeight); // Minimum 50px
    },
    [],
  );

  const getOneLevelUpperPIDs = React.useCallback(
    (pID: number, processResponse: ProcessData): Set<number> => {
      const IDs = new Set<number>();
      for (const r of processResponse.relations) {
        if (r.tag !== "Vertical") continue;
        if (r.vDown === pID) {
          IDs.add(r.vUp);
        }
      }
      return IDs;
    },
    [],
  );

  const processVisualizationData = React.useCallback(
    (
      timelinesResponse: ProcessTimelines<number>,
      processResponse: ProcessData,
    ) => {
      const functions: Map<string, Function> = new Map<string, Function>();

      const getAllLowerPIDs = (pID: number) => {
        const lowerIDs = new Set<number>();
        for (const r of processResponse.relations) {
          if (r.tag !== "Vertical") continue;
          if (r.vUp === pID) {
            lowerIDs.add(r.vDown);
            const lowerLevelIDs = getAllLowerPIDs(r.vDown);
            for (const lowerID of lowerLevelIDs) lowerIDs.add(lowerID);
          }
        }
        return lowerIDs;
      };

      const getAllUpperPIDs = (pID: number) => {
        const upperIDs = new Set<number>();
        for (const r of processResponse.relations) {
          if (r.tag !== "Vertical") continue;
          if (r.vDown === pID) {
            upperIDs.add(r.vUp);
            const upperLevelIDs = getAllUpperPIDs(r.vUp);
            for (const upperID of upperLevelIDs) upperIDs.add(upperID);
          }
        }
        return upperIDs;
      };

      timelinesResponse.timelines.forEach((timeline) => {
        const component = timeline.timelineViewpoint.component.join(".");
        if (timeline.timelineViewpoint.level === "Fun") {
          timeline.timelinePoints.forEach(
            (pointGroup: TimelinePoint<number>[]) => {
              pointGroup.forEach((point) => {
                const functionId = `${component}[${point.pTime[0]};${point.pTime[1]}]`;

                if (!functions.has(functionId)) {
                  const func: Function = {
                    label: point.pInfo.split(/do \w+: /)[1],
                    pID: point.pID,
                    component: component,
                    startTime: point.pTime[0],
                    endTime: point.pTime[1],
                    instructions: [],
                    lowerPIDs: getAllLowerPIDs(point.pID),
                    column: 0,
                    width: MIN_COLUMN_WIDTH,
                    isMemoryInit: false,
                  };
                  functions.set(functionId, func);
                }
              });
            },
          );
        }
      });

      const inputsPerInstructionMap = new Map<string, number>();
      const outputsPerInstructionMap = new Map<string, number>();

      timelinesResponse.timelines.forEach((timeline) => {
        const component = timeline.timelineViewpoint.component.join(".");
        if (timeline.timelineViewpoint.level === "INST") {
          timeline.timelinePoints.forEach(
            (pointGroup: TimelinePoint<number>[]) => {
              pointGroup.forEach((point) => {
                let funcFound = 0;
                for (const [, f] of functions) {
                  if (f.lowerPIDs.has(point.pID) && f.component === component) {
                    const upperPIDs = getOneLevelUpperPIDs(
                      point.pID,
                      processResponse,
                    );

                    const inputs = new Set<string>();
                    const outputs = new Set<string>();

                    for (const upperPointID of upperPIDs) {
                      for (const step of processResponse.steps) {
                        if (step.pID !== upperPointID) continue;
                        if (step.pDesc.includes(" Endpoint: ")) {
                          let separator: string;
                          if (step.pDesc.includes("Target")) {
                            separator = " Endpoint: Target ";
                            const args = step.pDesc
                              .split(separator)[1]
                              .split(",");
                            for (const arg of args) {
                              inputs.add(arg);
                              inputsPerInstructionMap.set(arg, point.pID);
                            }
                          } else {
                            separator = " Endpoint: Source ";
                            const args = step.pDesc
                              .split(separator)[1]
                              .split(",");
                            for (const arg of args) {
                              outputs.add(arg);
                              outputsPerInstructionMap.set(arg, point.pID);
                            }
                          }
                        }
                      }
                    }
                    const i: Instruction = {
                      pID: point.pID,
                      label: point.pInfo.split(": ")[1],
                      startTime: point.pTime[0],
                      endTime: point.pTime[1],
                      info: point.pInfo,
                      inputs: inputs,
                      outputs: outputs,
                      receiveInputsFromPIDs: new Map(),
                      sendsOutputsToPIDs: new Map(),
                      outputPosition: OutputPosition.None,
                    };
                    f.instructions.push(i);
                    funcFound += 1;
                  }
                }
                if (funcFound > 1)
                  console.log(
                    "TO MANY MATCHES " + point.pID + " " + point.pInfo,
                  );
                if (funcFound === 0)
                  console.log("NO MATCHES " + point.pID + " " + point.pInfo);
              });
            },
          );
        }
      });

      const estimateArrowTextWidth = (label: string): number => {
        // approximately 7px per character at font size 11, plus 4px padding
        return label.length * 7 + 4;
      };

      for (const f of functions.values()) {
        if (f.instructions.length > 0) {
          f.startTime = Math.min(...f.instructions.map((i) => i.startTime));
          f.endTime = Math.max(...f.instructions.map((i) => i.endTime));
        }
        for (const i of f.instructions) {
          for (const input of i.inputs) {
            const inputPID = outputsPerInstructionMap.get(input);
            if (inputPID) i.receiveInputsFromPIDs.set(input, inputPID);
          }
          for (const output of i.outputs) {
            const outputPID = inputsPerInstructionMap.get(output);
            if (outputPID) i.sendsOutputsToPIDs.set(output, outputPID);
          }
        }
        let maxInstructionWidth =
          f.instructions.length > 0
            ? Math.max(
                ...f.instructions.map((i) => i.label.length * 8 + TEXT_PADDING),
              )
            : MIN_COLUMN_WIDTH;
        maxInstructionWidth =
          f.instructions.length > 0
            ? Math.max(
                ...f.instructions.map(
                  (i) =>
                    `(${Array.from(i.inputs).join(",")}) -> (${Array.from(i.outputs).join(",")})`
                      .length *
                      8 +
                    TEXT_PADDING,
                ),
              )
            : MIN_COLUMN_WIDTH;

        let maxArrowTextWidth = 0;
        f.instructions.forEach((instr) => {
          instr.sendsOutputsToPIDs.forEach((targetId, variableName) => {
            const arrowTextWidth = estimateArrowTextWidth(variableName);
            maxArrowTextWidth = Math.max(maxArrowTextWidth, arrowTextWidth);
          });
        });

        maxInstructionWidth = Math.max(
          maxInstructionWidth,
          maxArrowTextWidth + TEXT_PADDING,
        );
        f.width = Math.max(MIN_COLUMN_WIDTH, maxInstructionWidth);
      }

      const functionsArray: Function[] = functions.values().toArray();
      functionsArray.sort((a, b) => a.startTime - b.startTime);

      type Interval = [number, number];
      const occupiedIntervalsPerColumn = new Map<number, Interval[]>();

      // assign columns to functions to prevent overlapping
      functionsArray.forEach((func) => {
        let assignedColumn = 0;
        const headerText = `${func.component} ${func.label}`;
        const estimatedHeaderHeight = estimateHeaderHeight(
          headerText,
          func.width,
        );
        const headerRowHeight = estimatedHeaderHeight / ROW_HEIGHT;

        while (true) {
          if (!occupiedIntervalsPerColumn.has(assignedColumn)) {
            const intervalArray: Interval[] = [];
            intervalArray.push([
              func.startTime - headerRowHeight,
              func.endTime + MIN_FUNCTION_GAP,
            ]);
            occupiedIntervalsPerColumn.set(assignedColumn, intervalArray);
            func.column = assignedColumn;
            break;
          }
          let isOverlapping = false;
          const intervals = occupiedIntervalsPerColumn.get(assignedColumn);
          if (intervals) {
            for (const interval of intervals) {
              const newFuncStart = func.startTime - headerRowHeight;
              const newFuncEnd = func.endTime + MIN_FUNCTION_GAP;
              if (!(newFuncEnd < interval[0] || newFuncStart > interval[1])) {
                isOverlapping = true;
                break;
              }
            }
          }
          if (isOverlapping) {
            assignedColumn += 1;
          } else {
            func.column = assignedColumn;
            const newInterval: Interval = [
              func.startTime - headerRowHeight,
              func.endTime + MIN_FUNCTION_GAP,
            ];
            occupiedIntervalsPerColumn.get(assignedColumn)?.push(newInterval);
            break;
          }
        }
      });

      functionsArray.sort(
        (a, b) => a.column - b.column || a.startTime - b.startTime,
      );

      const getInstructionColumnByPID = (PID: number) => {
        for (const f of functionsArray) {
          for (const i of f.instructions) {
            if (i.pID === PID) return f.column;
          }
        }
        return null;
      };

      functionsArray.forEach((f) => {
        f.instructions.forEach((i) => {
          for (const [_, targetPID] of i.sendsOutputsToPIDs) {
            const targetInstructionColumn =
              getInstructionColumnByPID(targetPID);
            if (targetInstructionColumn === null) continue;
            let nextOutputPosition: OutputPosition;
            if (targetInstructionColumn >= f.column)
              nextOutputPosition = OutputPosition.Right;
            else nextOutputPosition = OutputPosition.Left;

            if (i.outputPosition === OutputPosition.None) {
              i.outputPosition = nextOutputPosition;
            } else if (i.outputPosition !== nextOutputPosition) {
              i.outputPosition = OutputPosition.Both;
            }
          }
        });
      });

      let minTime = Math.min(...functionsArray.map((f) => f.startTime));
      const maxTime = Math.max(...functionsArray.map((f) => f.endTime));

      // сalculate max header height for functions starting at minTime to add to container height
      const functionsAtMinTime = functionsArray.filter(
        (f) => f.startTime === minTime,
      );
      const maxHeaderHeightAtMin = Math.max(
        ...functionsAtMinTime.map((f) =>
          estimateHeaderHeight(`${f.component} ${f.label}`, f.width),
        ),
        0,
      );

      // align function blocks to the most left position to minimize width
      const mostLeftFreeSpaceInColumnsByRows = new Map<
        number,
        Map<number, number>
      >();
      for (let i = minTime - 2; i <= maxTime; i++) {
        const columnsMap = new Map<number, number>();
        for (let i = 0; i < occupiedIntervalsPerColumn.size; i++)
          columnsMap.set(i, COLUMN_MARGIN);
        mostLeftFreeSpaceInColumnsByRows.set(i, columnsMap);
      }

      functionsArray.forEach((func) => {
        const estimatedHeaderHeight = estimateHeaderHeight(
          `${func.component} ${func.label}`,
          func.width,
        );
        const headerRowHeight = Math.ceil(estimatedHeaderHeight / ROW_HEIGHT);
        const firstAffectedRowIndex = Math.max(
          func.startTime - headerRowHeight,
          -1,
        );
        const lastAffectedRowIndex = func.endTime;

        const instructionPerRow = new Map<number, Instruction>();
        for (const instr of func.instructions) {
          for (let i = instr.startTime; i <= instr.endTime; i++) {
            instructionPerRow.set(i, instr);
          }
        }

        let currentLeftPositionOfFunction = 0;
        if (func.column !== 0) {
          for (
            let i = firstAffectedRowIndex;
            i < lastAffectedRowIndex + 1;
            i++
          ) {
            let leftArrowTextWidth = 0;
            let arrowLabel = "";
            if (instructionPerRow.has(i)) {
              const instr = instructionPerRow.get(i)!;
              arrowLabel = instr.sendsOutputsToPIDs.keys().toArray()[0];
              if (
                instr.outputPosition === OutputPosition.Left ||
                instr.outputPosition === OutputPosition.Both
              )
                leftArrowTextWidth = estimateArrowTextWidth(arrowLabel);
            }
            currentLeftPositionOfFunction = Math.max(
              currentLeftPositionOfFunction,
              mostLeftFreeSpaceInColumnsByRows.get(i)!.get(func.column - 1)! +
                leftArrowTextWidth,
            );
            const prevColumnWithLeftArrow =
              mostLeftFreeSpaceInColumnsByRows.get(i)!.get(func.column - 1)! +
              leftArrowTextWidth;
            mostLeftFreeSpaceInColumnsByRows
              .get(i)!
              .set(func.column - 1, prevColumnWithLeftArrow);
          }
        }
        for (let i = firstAffectedRowIndex; i < lastAffectedRowIndex + 1; i++) {
          let rightArrowTextWidth = 0;
          if (instructionPerRow.has(i)) {
            const instr = instructionPerRow.get(i)!;
            if (
              instr.outputPosition === OutputPosition.Right ||
              instr.outputPosition === OutputPosition.Both
            )
              rightArrowTextWidth = estimateArrowTextWidth(
                instr.sendsOutputsToPIDs.keys().toArray()[0],
              );
          }
          const spaceBetweenColumns =
            rightArrowTextWidth !== 0 ? rightArrowTextWidth : COLUMN_MARGIN;
          const nextColumnRightBorder =
            currentLeftPositionOfFunction + func.width + spaceBetweenColumns;
          mostLeftFreeSpaceInColumnsByRows
            .get(i)!
            .set(func.column, nextColumnRightBorder);
        }
      });

      const widthsMap = new Map<number, number>();
      functionsArray.forEach((func) => {
        const currentMaxWidth = widthsMap.get(func.column) || 0;
        let maxArrowTextWidth = 0;
        func.instructions.forEach((instr) => {
          instr.sendsOutputsToPIDs.forEach((targetId, variableName) => {
            const arrowTextWidth = estimateArrowTextWidth(variableName);
            maxArrowTextWidth = Math.max(maxArrowTextWidth, arrowTextWidth);
          });
        });
        widthsMap.set(
          func.column,
          Math.max(currentMaxWidth, func.width + maxArrowTextWidth),
        );
      });

      if (!isFinite(minTime)) minTime = 0;

      const connections: DataFlowConnection[] = [];

      functionsArray.forEach((func) => {
        func.instructions.forEach((instr) => {
          instr.sendsOutputsToPIDs.forEach((targetId, variableName) => {
            connections.push({
              sourceId: instr.pID,
              targetId,
              variableName,
            });
          });
        });
      });

      setFunctions(functionsArray);
      setTimelineConfig({ minTime, maxTime });
      setContainerHeight(
        maxHeaderHeightAtMin + (maxTime - minTime + 1) * ROW_HEIGHT,
      );
      setColumnWidths(widthsMap);
      setDataFlowConnections(connections);
      setMostLeftFreeSpacesInColumnsPerRows(mostLeftFreeSpaceInColumnsByRows);
    },
    [estimateHeaderHeight, getOneLevelUpperPIDs],
  );

  useEffect(() => {
    setLoading(true);
    setError(null);
    api
      .getTimelines(selectedSid)
      .then((timelinesResponse: any) => {
        return api.getProcess(selectedSid).then((processResponse: any) => ({
          timelines: timelinesResponse.data,
          process: processResponse.data,
        }));
      })
      .then(({ timelines, process }) => {
        processVisualizationData(timelines, process);
        setLoading(false);
      })
      .catch((err: any) => {
        console.error("Error loading visualization data:", err);
        const errorMsg =
          err?.response?.status === 404
            ? "No data available for this node"
            : err?.message || "Unknown error";
        setError(`Failed to load visualization data: ${errorMsg}`);
        setLoading(false);
      });
  }, [selectedSid, processVisualizationData]);

  if (loading) {
    return <div className="pt-4">Loading...</div>;
  }

  if (error) {
    return <div className="pt-4 alert alert-danger">{error}</div>;
  }

  if (functions.length === 0) {
    return <div className="pt-4">No timeline data to display</div>;
  }

  return (
    <div>
      <div className="legend-section">
        <div className="legend-container">
          {Array.from(selectedColorsRef.current.entries()).map(
            ([componentName, colorKey]) => (
              <div key={componentName} className="legend-item">
                <div
                  className="legend-color-swatch"
                  style={{
                    backgroundColor: COMPONENT_COLORS[colorKey].toHexString(),
                  }}
                />
                <span className="legend-label">{componentName}</span>
              </div>
            ),
          )}
        </div>
      </div>

      <div className="process-timelines-2-vertical">
        <div className="vertical-time-axis">
          <div className="time-labels">
            {Array.from(
              {
                length:
                  Math.ceil(timelineConfig.maxTime - timelineConfig.minTime) +
                  2,
              },
              (_, i) => timelineConfig.minTime + i - 1,
            ).map((time) => (
              <div
                key={time}
                className="time-label-item"
                style={{
                  top:
                    topPadding +
                    (time - timelineConfig.minTime) * ROW_HEIGHT +
                    (ROW_HEIGHT * (1 - 0.6)) / 2,
                  height: ROW_HEIGHT * 0.6,
                  width: ROW_HEIGHT * 0.6,
                }}
              >
                {time === timelineConfig.minTime - 1 ? `clk` : time}
              </div>
            ))}
          </div>
        </div>

        <div
          className="diagram-container"
          ref={containerRef}
          style={{
            minHeight: `${containerHeight}px`,
            paddingTop: `${topPadding}px`,
          }}
        >
          {/* SVG overlay for data flow arrows and grid lines */}
          <svg className="data-flow-overlay" role={"presentation"}>
            <defs>
              {Array.from(
                new Set(
                  dataFlowConnections.map((c) => (
                    <marker
                      key={`marker-404040`}
                      id={`arrowhead-404040`}
                      markerWidth="5"
                      markerHeight="10"
                      refX="5"
                      refY="3"
                      orient="auto"
                    >
                      <polygon points="0 0, 6 3, 0 6" fill="#404040" />
                    </marker>
                  )),
                ),
              )}
            </defs>
            {/* horizontal dotted grid lines */}
            {(() => {
              let totalWidth = 0;
              for (
                let col = 0;
                col < Math.max(...functions.map((f) => f.column), -1) + 1;
                col++
              ) {
                totalWidth +=
                  (columnWidths.get(col) || MIN_COLUMN_WIDTH) + COLUMN_MARGIN;
              }

              return Array.from(
                {
                  length:
                    Math.ceil(timelineConfig.maxTime - timelineConfig.minTime) +
                    1,
                },
                (_, i) => timelineConfig.minTime + i,
              ).map((time) => (
                <line
                  key={`grid-line-${time}`}
                  x1="0"
                  y1={topPadding + time * ROW_HEIGHT}
                  x2={totalWidth}
                  y2={topPadding + time * ROW_HEIGHT}
                  stroke="#40404060"
                  strokeDasharray="2,4"
                  strokeWidth="1"
                  pointerEvents="none"
                />
              ));
            })()}
            {/* render all arrow paths underneath */}
            {dataFlowConnections.map((connection, idx) => {
              const source = instructionPositions.get(connection.sourceId);
              const target = instructionPositions.get(connection.targetId);

              if (!source || !target) return null;

              const props = getArrowProps(
                source!,
                target!,
                topPadding,
                connection.variableName,
              );

              return (
                <ArrowPath
                  key={`arrow-path-${connection.sourceId}:${connection.targetId}`}
                  {...props}
                />
              );
            })}
            {/* render all arrow labels on top */}
            {dataFlowConnections.map((connection, idx) => {
              const source = instructionPositions.get(connection.sourceId);
              const target = instructionPositions.get(connection.targetId);

              if (!source || !target) return null;

              const props = getArrowProps(
                source!,
                target!,
                topPadding,
                connection.variableName,
              );

              return (
                <ArrowLabel
                  key={`arrow-label-${connection.sourceId}:${connection.targetId}`}
                  {...props}
                />
              );
            })}
          </svg>

          {functions.map((func, idx) => {
            const bgColor = getComponentColor(func.component);
            const bgTransparentColor = new Color({
              r: bgColor.obj.r,
              g: bgColor.obj.g,
              b: bgColor.obj.b,
            });
            bgTransparentColor.obj.a = 0x15 / 255;
            const headerHeight = headerHeights.get(func.pID) || ROW_HEIGHT;
            let leftPosition = 0;

            for (
              let i = Math.max(
                func.startTime - Math.ceil(headerHeight / ROW_HEIGHT),
                -1,
              );
              i <= func.endTime;
              i++
            )
              leftPosition = Math.max(
                leftPosition,
                mostLeftFreeSpacesInColumnsPerRows
                  .get(i)!
                  .get(func.column - 1)!,
              );

            return (
              <div
                key={func.pID}
                className="function-rectangle"
                style={{
                  top:
                    topPadding +
                    (func.startTime - timelineConfig.minTime) * ROW_HEIGHT -
                    headerHeight,
                  height:
                    (func.endTime - func.startTime + 1) * ROW_HEIGHT +
                    headerHeight,
                  left: `${leftPosition}px`,
                  width: `${func.width}px`,
                  borderColor: bgColor.toHexString(),
                  backgroundColor: bgTransparentColor.toHexString(),
                }}
              >
                <div
                  data-header-id={`func-header-${func.pID}`}
                  className="function-header"
                  style={{
                    backgroundColor: fadeColor(
                      bgColor,
                      0x15 / 255,
                    ).toHexString(), // header function background is not transparent but faded
                    color: bgColor.toHexString(),
                    height: "auto",
                    minHeight: `${headerHeight}px`,
                  }}
                >
                  <div className="function-name">
                    <strong>{func.component}</strong> {func.label}
                  </div>
                  <div className="function-time">
                    [{func.startTime};{func.endTime}]
                  </div>
                </div>

                <div className="instructions-container">
                  {func.instructions.map((instr) => {
                    return (
                      <div
                        key={instr.pID}
                        data-instruction-id={`instr-${instr.pID}`}
                        className="instruction-rectangle"
                        style={{
                          top:
                            (instr.startTime - func.startTime) * ROW_HEIGHT + 8,
                          height: Math.max(
                            50,
                            (instr.endTime - instr.startTime) * ROW_HEIGHT - 16,
                          ),
                          border: `2px solid ${bgColor.toHexString()}`,
                        }}
                        title={instr.info}
                      >
                        <div className="instruction-content">
                          <div className="instruction-label">
                            <strong>
                              {instr.label} #{instr.pID}
                            </strong>
                          </div>
                          <div className="instruction-time">
                            [{instr.startTime};{instr.endTime}]
                          </div>
                          <div className="instruction-io-info">
                            ({Array.from(instr.inputs).join(",")}) -{">"} (
                            {Array.from(instr.outputs).join(",")})
                          </div>
                        </div>
                      </div>
                    );
                  })}
                </div>
              </div>
            );
          })}
        </div>
      </div>
      <JsonView
        style={{ gap: "2rem", padding: "3rem 3rem 3rem 4rem" }}
        value={functions}
        collapsed={1}
        shortenTextAfterLength={120}
      />
    </div>
  );
};
