import type { ProcessTimelines, TimelinePoint } from "services/gen/types";
import type { ProcessData } from "services/HaskellApiService";

export enum OutputPosition {
  Left,
  Right,
  Both,
  None,
}

export interface Instruction {
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
  isMemoryInit: boolean;
}

export interface DataFlowConnection {
  sourceId: number;
  targetId: number;
  variableName: string;
}

export const MIN_COLUMN_WIDTH = 50;
export const TEXT_PADDING = 40;

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
): { functions: ProcessFunction[]; dataFlowConnections: DataFlowConnection[] } {
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
              isMemoryInit: false,
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
                receiveInputsFromPIDs: new Map(),
                sendsOutputsToPIDs: new Map(),
                outputPosition: OutputPosition.None,
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

  const functionsArray: ProcessFunction[] = functionsMap.values().toArray();

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

  return { functions: functionsArray, dataFlowConnections: connections };
}
