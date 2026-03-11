import { AppContext, type IAppContext } from "app/AppContext";
import React, {
  type FC,
  useCallback,
  useContext,
  useEffect,
  useLayoutEffect,
  useState,
} from "react";
import type { ProcessTimelines } from "services/gen/types";
import { api, type ProcessData } from "services/HaskellApiService";
import "components/ProcessTimeline2.scss";
import { TimelinePerUnit } from "./ProcessTimeline2/TimelinePerUnit";
import { SplitPane } from "./utils/SplitPane";
import { COMPONENT_COLORS, Color, fadeColor } from "../utils/color";
import { JsonView } from "./JsonView";
import {
  type InstructionPosition,
} from "./utils/ArrowWithLabel";
import {
  type DataFlowConnection,
  estimateArrowTextWidth,
  type Instruction,
  OutputPosition,
  type ProcessFunction,
  parseProcessData,
} from "./utils/ProcessTimeline2";
import { FunctionRectangle } from "./ProcessTimeline2/FunctionBlockComponents";
import { DataFlowOverlay } from "./ProcessTimeline2/TimelineView";

const ROW_HEIGHT = 70;
const COLUMN_MARGIN = 20;
const MIN_FUNCTION_GAP = 0.5;
const CONTAINER_BUTTOM_PADDING = ROW_HEIGHT;

export const ProcessTimelines2: FC = () => {
  const { selectedSid } = useContext(AppContext) as IAppContext;

  const [functions, setFunctions] = useState<ProcessFunction[]>([]);
  const [timelineConfig, setTimelineConfig] = useState({
    minTime: 0,
    maxTime: 10,
  });
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [containerHeight, setContainerHeight] = useState(1000);
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
  const [topPadding, setTopPadding] = useState(0);
  const [layoutCalculated, setLayoutCalculated] = useState(false);

  const getComponentColor = useCallback((component: string): Color => {
    const preselectedColor = selectedColorsRef.current.get(component);
    if (preselectedColor) return COMPONENT_COLORS[preselectedColor];

    const hashCode = component
      .split("[")[0]
      .split("")
      .reduce((hash, char) => ((hash << 5) - hash + char.charCodeAt(0)) | 0, 0);

    const occupiedColors = new Set(selectedColorsRef.current.values());

    const freeColors = Object.keys(COMPONENT_COLORS).filter(
      (k) =>
        k !== "default" &&
        (!occupiedColors.has(k) ||
          occupiedColors.size === Object.keys(COMPONENT_COLORS).length),
    );

    const selectedColor = freeColors[Math.abs(hashCode) % freeColors.length];
    selectedColorsRef.current.set(component, selectedColor);
    return COMPONENT_COLORS[selectedColor];
  }, []);

  const mapsEqual = useCallback(
    (map1: Map<number, number>, map2: Map<number, number>): boolean => {
      if (map1.size !== map2.size) return false;
      for (const [key, value] of map1) {
        if (map2.get(key) !== value) return false;
      }
      return true;
    },
    [],
  );

  const calculateHeaderHeights = useCallback(() => {
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
        heightsMap.set(func.pID, Math.max(measuredHeight, 50));
      } else {
        heightsMap.set(func.pID, ROW_HEIGHT);
      }
    });

    setHeaderHeights((prevHeights) => {
      if (mapsEqual(prevHeights, heightsMap)) {
        return prevHeights;
      }
      return heightsMap;
    });
  }, [functions, mapsEqual]);

  const instructionPositionsEqual = useCallback(
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

  const calculateInstructionPositions = useCallback(() => {
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

    setInstructionPositions((prevPositions) => {
      if (instructionPositionsEqual(prevPositions, positionsMap)) {
        return prevPositions;
      }
      return positionsMap;
    });
  }, [functions, getComponentColor, instructionPositionsEqual]);

  const parseProcessDataLocal = useCallback(
    (
      timelinesResponse: ProcessTimelines<number>,
      processResponse: ProcessData,
    ) => {
      const { functions: functionsArray, dataFlowConnections: connections } =
        parseProcessData(timelinesResponse, processResponse);

      let minTime = Math.min(...functionsArray.map((f) => f.startTime));
      const maxTime = Math.max(...functionsArray.map((f) => f.endTime));
      if (!isFinite(minTime)) minTime = 0;

      const initialMostLeftSpaces = new Map<number, Map<number, number>>();
      for (let i = minTime - 2; i <= maxTime; i++) {
        const columnsMap = new Map<number, number>();
        columnsMap.set(-1, 0);
        columnsMap.set(0, COLUMN_MARGIN);
        initialMostLeftSpaces.set(i, columnsMap);
      }

      setFunctions(functionsArray);
      setTimelineConfig({ minTime, maxTime });
      setContainerHeight((maxTime - minTime + 1) * ROW_HEIGHT + 100);
      setDataFlowConnections(connections);
      setMostLeftFreeSpacesInColumnsPerRows(initialMostLeftSpaces);
      setLayoutCalculated(false);
    },
    [],
  );

  const performLayout = useCallback(() => {
    if (functions.length === 0 || headerHeights.size < functions.length) return;

    const functionsArray = functions.map((f) => ({ ...f }));
    functionsArray.sort((a, b) => a.startTime - b.startTime);

    type Interval = [number, number];
    const occupiedIntervalsPerColumn = new Map<number, Interval[]>();

    // place functions into columns
    functionsArray.forEach((func) => {
      let assignedColumn = 0;
      const headerHeight = headerHeights.get(func.pID) || ROW_HEIGHT;
      const headerRowHeight = Math.ceil(headerHeight / ROW_HEIGHT);

      while (true) {
        if (!occupiedIntervalsPerColumn.has(assignedColumn)) {
          const intervalArray: Interval[] = [];
          intervalArray.push([
            Math.max(func.startTime - headerRowHeight, -1),
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
        for (const [, targetPID] of i.sendsOutputsToPIDs) {
          const targetInstructionColumn = getInstructionColumnByPID(targetPID);
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

    const { minTime, maxTime } = timelineConfig;

    // align function blocks to the most left position to minimize width
    const mostLeftFreeSpaceInColumnsByRows = new Map<
      number,
      Map<number, number>
    >();
    const maxCol = Math.max(...functionsArray.map((f) => f.column), 0);

    for (let i = minTime - 2; i <= maxTime; i++) {
      const columnsMap = new Map<number, number>();
      columnsMap.set(-1, 0);
      for (let j = 0; j <= maxCol; j++) columnsMap.set(j, COLUMN_MARGIN);
      mostLeftFreeSpaceInColumnsByRows.set(i, columnsMap);
    }

    functionsArray.forEach((func) => {
      const headerHeight = headerHeights.get(func.pID) || ROW_HEIGHT;
      const headerRowHeight = Math.ceil(headerHeight / ROW_HEIGHT);

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
        for (let i = firstAffectedRowIndex; i < lastAffectedRowIndex + 1; i++) {
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

    const functionsAtMinTime = functionsArray.filter(
      (f) => f.startTime === minTime,
    );
    const maxHeaderHeightAtMin = Math.max(
      ...functionsAtMinTime.map((f) => headerHeights.get(f.pID) || ROW_HEIGHT),
      0,
    );

    setFunctions(functionsArray);
    setContainerHeight(
      maxHeaderHeightAtMin +
        (maxTime - minTime + 1) * ROW_HEIGHT +
        CONTAINER_BUTTOM_PADDING,
    );
    setMostLeftFreeSpacesInColumnsPerRows(mostLeftFreeSpaceInColumnsByRows);
    setTopPadding(maxHeaderHeightAtMin);
    setLayoutCalculated(true);
  }, [headerHeights, timelineConfig, functions.length, functions.map]);

  useEffect(() => {
    if (selectedSid) {
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
          parseProcessDataLocal(timelines, process);
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
    }
  }, [selectedSid, parseProcessDataLocal]);

  useLayoutEffect(() => {
    if (!loading && functions.length > 0) {
      calculateHeaderHeights();
    }
  }, [functions, loading, calculateHeaderHeights]);

  useLayoutEffect(() => {
    performLayout();
  }, [performLayout]);

  useLayoutEffect(() => {
    if (layoutCalculated) calculateInstructionPositions();
  }, [layoutCalculated, calculateInstructionPositions]);

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
          {/*<div className="time-labels">*/}
          {Array.from(
            {
              length:
                Math.ceil(timelineConfig.maxTime - timelineConfig.minTime) + 2,
            },
            (_, i) => timelineConfig.minTime + i - 1,
          ).map((time) => (
            <div
              key={time}
              className="time-label-item"
              style={{
                marginTop:
                  time === timelineConfig.minTime - 1
                    ? topPadding - ROW_HEIGHT + ROW_HEIGHT * 0.2
                    : ROW_HEIGHT * 0.2,
                marginBottom: ROW_HEIGHT * 0.2,
                height: ROW_HEIGHT * 0.6,
                width: ROW_HEIGHT * 0.6,
              }}
            >
              {time === timelineConfig.minTime - 1 ? `clk` : time}
            </div>
          ))}
          {/*</div>*/}
        </div>

        <div className="diagram-split-view">
          <SplitPane initialSplitPercentage={70} minWidthLeft={300} minWidthRight={200}>
            <div
              className="diagram-container"
              ref={containerRef}
              style={{
                minHeight: `${containerHeight}px`,
                paddingTop: `${topPadding}px`,
              }}
            >
              {/* SVG overlay for data flow arrows and grid lines */}
              <DataFlowOverlay
                dataFlowConnections={dataFlowConnections}
                instructionPositions={instructionPositions}
                topPadding={topPadding}
                timelineConfig={timelineConfig}
                // mostLeftFreeSpacesInColumnsPerRows={mostLeftFreeSpacesInColumnsPerRows}
                rowHeight={ROW_HEIGHT}
              />

              {functions.map((func) => {
                const bgColor = getComponentColor(func.component);
                const headerHeight = headerHeights.get(func.pID) || ROW_HEIGHT;

                return (
                  <FunctionRectangle
                    key={func.pID}
                    func={func}
                    bgColor={bgColor}
                    headerHeight={headerHeight}
                    rowHeight={ROW_HEIGHT}
                    topPadding={topPadding}
                    timelineConfig={timelineConfig}
                    mostLeftFreeSpacesInColumnsPerRows={mostLeftFreeSpacesInColumnsPerRows}
                  />
                );
              })}
            </div>

            <TimelinePerUnit
              functions={functions}
              timelineConfig={timelineConfig}
              rowHeight={ROW_HEIGHT}
              topPadding={topPadding}
              containerHeight={containerHeight}
              getComponentColor={getComponentColor}
              // dataFlowConnections={dataFlowConnections}
              // instructionPositions={instructionPositions}
              // mostLeftFreeSpacesInColumnsPerRows={mostLeftFreeSpacesInColumnsPerRows}
            />
          </SplitPane>
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
