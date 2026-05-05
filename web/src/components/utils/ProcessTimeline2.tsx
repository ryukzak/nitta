import type React from "react";
import { useCallback } from "react";
import type { ProcessTimelines, TimelinePoint } from "services/gen/types";
import type { ProcessData } from "services/HaskellApiService";
import type { InstructionPosition } from "./ArrowWithLabel";

export enum LabelPosition {
  Left,
  Right,
  Both,
  None,
  Center,
}

export interface Instruction {
  pID: number;
  label: string;
  startTime: number;
  endTime: number;
  info: string;
  inputs: Set<string>;
  outputs: Set<string>;
  outputPositions: Map<string, number>;
  inputPositions: Map<string, LabelPosition>;
  receiveInputsFromPIDs: Map<string, number>;
  sendsOutputsToPIDs: Map<string, number>;
  outputPosition: LabelPosition;
}

export interface ProcessFunction {
  label: string;
  component: string;
  pID: number;
  startTime: number;
  endTime: number;
  instructions: Instruction[];
  lowerPIDs: Set<number>;
  column: number;
  width: number;
  leftPosition: number;
  instructionMaxWidth: number;
  isMemoryInit: boolean;
}

export interface DataFlowConnection {
  sourceId: number | null;
  targetId: number | null;
  variableName: string;
  isSourcePlanned: boolean;
  isTargetPlanned: boolean;
}

export interface Unit {
  name: string;
  subunits: Unit[] | null;
  functions: ProcessFunction[] | null;
}

export const MIN_COLUMN_WIDTH = 50;
export const TEXT_PADDING = 40;
export const ROW_HEIGHT = 70;
export const COLUMN_MARGIN = 20;
export const MIN_FUNCTION_GAP = 0.5;

export const estimateArrowTextWidth = (label: string): number => {
  // approximately 7px per character at font size 11, plus 4px padding
  return label.length * 7 + 4;
};

export const getOneLevelUpperPIDs = (
  pID: number,
  processResponse: ProcessData,
): Set<number> => {
  const IDs = new Set<number>();
  for (const r of processResponse.relations) {
    if (r.tag !== "Vertical") continue;
    if (r.vDown === pID) {
      IDs.add(r.vUp);
    }
  }
  return IDs;
};

export function parseProcessData(
  timelinesResponse: ProcessTimelines<number>,
  processResponse: ProcessData,
): {
  functions: ProcessFunction[];
  dataFlowConnections: DataFlowConnection[];
  units: Unit[];
} {
  const functionsMap: Map<string, ProcessFunction> = new Map<
    string,
    ProcessFunction
  >();

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

  timelinesResponse.timelines.forEach((timeline) => {
    const component = timeline.timelineViewpoint.component.join(".");
    if (timeline.timelineViewpoint.level === "Fun") {
      timeline.timelinePoints.forEach((pointGroup: TimelinePoint<number>[]) => {
        pointGroup.forEach((point) => {
          const functionId = `${component}[${point.pTime[0]};${point.pTime[1]}]`;

          if (!functionsMap.has(functionId)) {
            const func: ProcessFunction = {
              label: point.pInfo.split(/do \w+: /)[1],
              pID: point.pID,
              component: component,
              startTime: point.pTime[0],
              endTime: point.pTime[1],
              instructions: [],
              lowerPIDs: getAllLowerPIDs(point.pID),
              column: 0,
              width: MIN_COLUMN_WIDTH,
              instructionMaxWidth: -1,
              isMemoryInit: false,
              leftPosition: 0,
            };
            functionsMap.set(functionId, func);
          }
        });
      });
    }
  });

  const inputsPerInstructionMap = new Map<string, number>();
  const outputsPerInstructionMap = new Map<string, number>();

  timelinesResponse.timelines.forEach((timeline) => {
    const component = timeline.timelineViewpoint.component.join(".");
    if (timeline.timelineViewpoint.level === "INST") {
      timeline.timelinePoints.forEach((pointGroup: TimelinePoint<number>[]) => {
        pointGroup.forEach((point) => {
          let funcFound = 0;
          for (const [, f] of functionsMap) {
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
                      const args = step.pDesc.split(separator)[1].split(",");
                      for (const arg of args) {
                        inputs.add(arg);
                        inputsPerInstructionMap.set(arg, point.pID);
                      }
                    } else {
                      separator = " Endpoint: Source ";
                      const args = step.pDesc.split(separator)[1].split(",");
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
                inputPositions: new Map(),
                outputPositions: new Map(),
                receiveInputsFromPIDs: new Map(),
                sendsOutputsToPIDs: new Map(),
                outputPosition: LabelPosition.None,
              };
              f.instructions.push(i);
              funcFound += 1;
            }
          }
        });
      });
    }
  });

  for (const f of functionsMap.values()) {
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

    const maxInstructionNameWidth =
      f.instructions.length > 0
        ? Math.max(
            ...f.instructions.map((i) => i.label.length * 8 + TEXT_PADDING),
          )
        : MIN_COLUMN_WIDTH;
    const maxInstructionIOWidth =
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

    let maxInstructionWidth = Math.max(
      maxInstructionNameWidth,
      maxInstructionIOWidth,
    );

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
    f.instructionMaxWidth = maxInstructionWidth - TEXT_PADDING;
  }

  const functionsArray: ProcessFunction[] = functionsMap.values().toArray();

  const connections: DataFlowConnection[] = [];
  functionsArray.forEach((func) => {
    func.instructions.forEach((instr) => {
      instr.sendsOutputsToPIDs.forEach((targetId, variableName) => {
        connections.push({
          sourceId: instr.pID,
          targetId,
          variableName,
          isSourcePlanned: true,
          isTargetPlanned: true,
        });
      });
    });
  });

  functionsArray.forEach((func) => {
    func.instructions.forEach((instr) => {
      instr.inputs.forEach((inputVarName) => {
        if (!instr.receiveInputsFromPIDs.has(inputVarName)) {
          connections.push({
            sourceId: null,
            targetId: instr.pID,
            variableName: inputVarName,
            isSourcePlanned: false,
            isTargetPlanned: true,
          });
        }
      });

      instr.outputs.forEach((outputVarName) => {
        if (!instr.sendsOutputsToPIDs.has(outputVarName)) {
          connections.push({
            sourceId: instr.pID,
            targetId: null,
            variableName: outputVarName,
            isSourcePlanned: true,
            isTargetPlanned: false,
          });
        }
      });
    });
  });

  const unitsMap = new Map<string, Unit>();
  functionsArray.forEach((f) => {
    if (!unitsMap.has(f.component)) {
      const u: Unit = { name: f.component, functions: [f], subunits: null };
      unitsMap.set(f.component, u);
    } else {
      unitsMap.get(f.component)!.functions!.push(f);
    }
  });

  unitsMap.forEach((u) => {
    if (u.name.includes("fram")) {
      const subunitsMap = new Map<string, Unit>();
      u.functions!.forEach((f) => {
        let subunitName: string | undefined;
        f.instructions.forEach((i) => {
          subunitName = "#" + i.label.charAt(i.label.length - 1);
        });
        if (subunitName === undefined) return;
        if (!subunitsMap.has(subunitName)) {
          const subunit: Unit = {
            name: subunitName,
            subunits: null,
            functions: [f],
          };
          subunitsMap.set(subunitName, subunit);
        } else {
          subunitsMap.get(subunitName)!.functions!.push(f);
        }
      });
      u.functions = null;
      u.subunits = [...subunitsMap.values()];
      u.subunits.sort((a, b) => a.name.localeCompare(b.name));
    }
  });

  const units = Array.from(unitsMap.values());

  return {
    functions: functionsArray,
    dataFlowConnections: connections,
    units: units,
  };
}

export function instructionPositionsEqual(
  map1: Map<number, InstructionPosition>,
  map2: Map<number, InstructionPosition>,
): boolean {
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
}

/**
 * Compare two maps with numeric keys and values for equality
 */
export const mapsEqual = (
  map1: Map<number, number>,
  map2: Map<number, number>,
): boolean => {
  if (map1.size !== map2.size) return false;
  for (const [key, value] of map1) {
    if (map2.get(key) !== value) return false;
  }
  return true;
};

/**
 * Get the instruction's column by searching through functions
 */
export const getInstructionColumnByPID = (
  pID: number,
  functions: ProcessFunction[],
): number | null => {
  for (const f of functions) {
    for (const i of f.instructions) {
      if (i.pID === pID) return f.column;
    }
  }
  return null;
};

/**
 * Determine arrow label position based on source and target columns
 */
export const getArrowLabelPosition = (
  fromColumn: number,
  toColumn: number,
): LabelPosition => {
  return toColumn >= fromColumn ? LabelPosition.Right : LabelPosition.Left;
};

/**
 * Assign input/output label positions to instructions based on column relationships
 */
export const assignInputOutputPositions = (
  functions: ProcessFunction[],
): void => {
  functions.forEach((f) => {
    f.instructions.forEach((i) => {
      i.inputs.forEach((inp) => {
        const targetInstructionPID = i.receiveInputsFromPIDs.get(inp);
        let inputPosition: LabelPosition;
        if (
          targetInstructionPID === undefined ||
          getInstructionColumnByPID(targetInstructionPID, functions) === null
        ) {
          inputPosition = LabelPosition.Left;
        } else {
          inputPosition = getArrowLabelPosition(
            f.column,
            getInstructionColumnByPID(targetInstructionPID, functions)!,
          );
        }
        i.inputPositions.set(inp, inputPosition);
      });
      i.outputs.forEach((outp) => {
        const targetInstructionPID = i.sendsOutputsToPIDs.get(outp);
        let outputPosition: LabelPosition;
        if (
          targetInstructionPID === undefined ||
          getInstructionColumnByPID(targetInstructionPID, functions) === null
        ) {
          outputPosition = LabelPosition.Right;
        } else {
          outputPosition = getArrowLabelPosition(
            f.column,
            getInstructionColumnByPID(targetInstructionPID, functions)!,
          );
        }
        i.outputPositions.set(outp, outputPosition);
      });
    });
  });
};

/**
 * Calculate instruction positions from DOM elements
 */
export const calculateInstructionPositionsFromDOM = (
  container: HTMLElement | null,
  functions: ProcessFunction[],
  getComponentColor: (component: string) => any,
  getColumn: (func: ProcessFunction) => number,
  scale: number = 1,
): Map<number, InstructionPosition> => {
  const positionsMap = new Map<number, InstructionPosition>();
  if (!container) return positionsMap;

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
        const relativeX = rect.left - containerRect.left + container.scrollLeft;
        const relativeY =
          rect.top - containerRect.top + container.scrollTop - paddingTop;

        positionsMap.set(instr.pID, {
          instructionId: instr.pID,
          x: relativeX / scale,
          y: relativeY / scale,
          width: rect.width / scale,
          height: rect.height / scale,
          color: getComponentColor(func.component).toHexString(),
          column: getColumn(func),
        });
      }
    });
  });

  return positionsMap;
};

export function createContainerClickHandler(
  containerRef: React.RefObject<HTMLElement | null>,
  onClearSelection: () => void,
) {
  return (e: React.MouseEvent<HTMLElement>) => {
    if (!containerRef) return;
    const target = e.target as HTMLElement;

    let isOnInteractive = false;
    let currentElement: Element | null = target;
    while (currentElement && currentElement !== containerRef.current) {
      if (
        currentElement.classList.contains("instruction-rectangle") ||
        currentElement.classList.contains("function-rectangle") ||
        currentElement.classList.contains("dataflow-group") ||
        currentElement.tagName === "polyline" ||
        currentElement.tagName === "path"
      ) {
        isOnInteractive = true;
        break;
      }
      currentElement = currentElement.parentElement;
    }
    if (!isOnInteractive) {
      onClearSelection();
    }
  };
}

/**
 * Create a zero-initialized map for a time range
 */
export const createZeroMap = (
  minTime: number,
  maxTime: number,
): Map<number, number> => {
  return new Map<number, number>(
    Array.from({ length: maxTime - minTime + 1 }, (_, i) => [minTime + i, 0]),
  );
};
