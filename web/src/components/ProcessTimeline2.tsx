import { AppContext, type IAppContext } from "app/AppContext";
import React, {
  type FC,
  useCallback,
  useContext,
  useEffect,
  useState,
} from "react";
import type { ProcessTimelines } from "services/gen/types";
import { api, type ProcessData } from "services/HaskellApiService";
import "components/ProcessTimeline2.scss";
import { COMPONENT_COLORS, type Color } from "../utils/color";
import { JsonView } from "./JsonView";
import type { DataFlow } from "./MicroarchitectureView";
import { ClickableIntermediateView } from "./ProcessTimeline2/ClickableIntermediateView";
import { ClickableMicroarchitectureView } from "./ProcessTimeline2/ClickableMicroarchitectureView";
import { FunctionTimeline } from "./ProcessTimeline2/FunctionTimeline";
import { UnitLabel } from "./ProcessTimeline2/UnitLabel";
import { UnitTimeline } from "./ProcessTimeline2/UnitTimeline";
import {
  COLUMN_MARGIN,
  type DataFlowConnection,
  type ProcessFunction,
  parseProcessData,
  ROW_HEIGHT,
  type Unit,
} from "./utils/ProcessTimeline2";
import { SplitPane } from "./utils/SplitPane";

export const ProcessTimelines2: FC = () => {
  const { selectedSid } = useContext(AppContext) as IAppContext;

  const [functions, setFunctions] = useState<ProcessFunction[]>([]);
  const [units, setUnits] = useState<Unit[]>([]);
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
  const selectedColorsRef = React.useRef<Map<string, string>>(new Map());
  const [topPadding, setTopPadding] = useState(0);
  const [enabledUnits, setEnabledUnits] = useState<Set<string>>(new Set());
  const [enabledFunctions, setEnabledFunctions] = useState<Set<string>>(
    new Set(),
  );
  const [filterOperator, setFilterOperator] = useState<"AND" | "OR">("AND");
  const [filteredFunctions, setFilteredFunctions] = useState<ProcessFunction[]>(
    [],
  );
  const [filteredUnits, setFilteredUnits] = useState<Unit[]>([]);
  const [showIntermediateView, setShowIntermediateView] = useState(false);
  const [selectedInstructionId, setSelectedInstructionId] = useState<
    number | null
  >(null);
  const [selectedDataFlowId, setSelectedDataFlowId] = useState<string | null>(
    null,
  );

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

  const handleUnitToggle = useCallback((unitName: string) => {
    setEnabledUnits((prev) => {
      const newSet = new Set(prev);
      if (newSet.has(unitName)) {
        newSet.delete(unitName);
      } else {
        newSet.add(unitName);
      }
      return newSet;
    });
  }, []);

  const handleFunctionToggle = useCallback((label: string) => {
    setEnabledFunctions((prev) => {
      const newSet = new Set(prev);
      if (newSet.has(label)) {
        newSet.delete(label);
      } else {
        newSet.add(label);
      }
      return newSet;
    });
  }, []);

  const handleToggleAllUnits = useCallback(() => {
    const allUnits = new Set(functions.map((f) => f.component));
    const allSelected =
      allUnits.size > 0 && allUnits.size === enabledUnits.size;
    setEnabledUnits(allSelected ? new Set() : allUnits);
  }, [functions, enabledUnits]);

  const handleToggleAllFunctions = useCallback(() => {
    const allFunctions = new Set(functions.map((f) => f.label));
    const allSelected =
      allFunctions.size > 0 && allFunctions.size === enabledFunctions.size;
    setEnabledFunctions(allSelected ? new Set() : allFunctions);
  }, [functions, enabledFunctions]);

  const handleInstructionSelect = useCallback((instructionId: number) => {
    setSelectedInstructionId(instructionId);
    setSelectedDataFlowId(null);
  }, []);

  const handleDataFlowSelect = useCallback((dataFlowId: string) => {
    setSelectedDataFlowId(dataFlowId);
    setSelectedInstructionId(null);
  }, []);

  const handleClearSelection = useCallback(() => {
    setSelectedInstructionId(null);
    setSelectedDataFlowId(null);
  }, []);

  const getRelatedDataFlows = useCallback(
    (instructionId: number): string[] => {
      return dataFlowConnections
        .map((conn, idx) => {
          if (
            conn.sourceId === instructionId ||
            conn.targetId === instructionId
          ) {
            return `${conn.sourceId}:${conn.targetId}`;
          }
          return null;
        })
        .filter((id): id is string => id !== null);
    },
    [dataFlowConnections],
  );

  const getRelatedInstructions = useCallback((dataFlowId: string): number[] => {
    const [sourceId, targetId] = dataFlowId
      .split(":")
      .map((id) => (id === "null" ? null : parseInt(id)));
    const instructions = [];
    if (sourceId !== null) instructions.push(sourceId);
    if (targetId !== null) instructions.push(targetId);
    return instructions;
  }, []);

  const getRelatedFunctionLabels = useCallback(
    (instructionId: number | null, dataFlowId: string | null): Set<string> => {
      const relatedLabels = new Set<string>();

      if (instructionId !== null) {
        for (const func of functions) {
          for (const instr of func.instructions) {
            if (instr.pID === instructionId) {
              relatedLabels.add(func.label);
              instr.sendsOutputsToPIDs.forEach((targetPID) => {
                for (const f of functions) {
                  for (const i of f.instructions) {
                    if (i.pID === targetPID) {
                      relatedLabels.add(f.label);
                    }
                  }
                }
              });
              instr.receiveInputsFromPIDs.forEach((sourcePID) => {
                for (const f of functions) {
                  for (const i of f.instructions) {
                    if (i.pID === sourcePID) {
                      relatedLabels.add(f.label);
                    }
                  }
                }
              });
              break;
            }
          }
        }
      } else if (dataFlowId !== null) {
        const relatedInstructions = getRelatedInstructions(dataFlowId);
        for (const instrId of relatedInstructions) {
          for (const func of functions) {
            for (const instr of func.instructions) {
              if (instr.pID === instrId) {
                relatedLabels.add(func.label);
              }
            }
          }
        }
      }

      return relatedLabels;
    },
    [functions, getRelatedInstructions],
  );

  const getRelatedDataFlowVariables = useCallback(
    (instructionId: number | null, dataFlowId: string | null): Set<string> => {
      const relatedVariables = new Set<string>();

      if (instructionId !== null) {
        for (const func of functions) {
          for (const instr of func.instructions) {
            if (instr.pID === instructionId) {
              instr.outputs.forEach((output) => {
                relatedVariables.add(output);
              });
              instr.inputs.forEach((input) => {
                relatedVariables.add(input);
              });
              break;
            }
          }
        }
      } else if (dataFlowId !== null) {
        for (const conn of dataFlowConnections) {
          if (`${conn.sourceId}:${conn.targetId}` === dataFlowId) {
            relatedVariables.add(conn.variableName);
          }
        }
      }

      return relatedVariables;
    },
    [functions, dataFlowConnections],
  );

  const getRelatedDataFlowVariablesForMicroarchitectureView = useCallback(
    (
      instructionId: number | null,
      dataFlowId: string | null,
    ): Set<DataFlow> => {
      const dataFlows = new Set<DataFlow>();

      const sourceInstructionIds = new Set<number>();
      const targetInstructionIds = new Set<number>();

      const getUnitNameByInstructionId = (instrId: number) => {
        for (const func of functions) {
          for (const instr of func.instructions) {
            if (instr.pID === instrId) {
              return func.component;
            }
          }
        }
        return null;
      };

      if (instructionId !== null) {
        for (const func of functions) {
          for (const instr of func.instructions) {
            if (instr.pID === instructionId) {
              const outputMap = new Map<string, string>();
              instr.sendsOutputsToPIDs.forEach((targetInstrId, varName) => {
                const componentName = getUnitNameByInstructionId(targetInstrId);
                outputMap.set(componentName!, varName);
              });
              if (outputMap.size > 0) {
                dataFlows.add({
                  source: func.component,
                  targetsWithVars: outputMap,
                  net: "net1",
                });
              }
              let inputId = -1;
              let inputVarName = "";
              instr.receiveInputsFromPIDs.forEach((sourceInstrId, varName) => {
                inputId = sourceInstrId;
                inputVarName = varName;
              });
              if (inputId !== -1) {
                const m = new Map<string, string>();
                m.set(func.component, inputVarName);
                dataFlows.add({
                  source: getUnitNameByInstructionId(inputId)!,
                  targetsWithVars: m,
                  net: "net1",
                });
              }
              break;
            }
          }
        }
      } else if (dataFlowId !== null) {
        for (const conn of dataFlowConnections) {
          if (`${conn.sourceId}:${conn.targetId}` === dataFlowId) {
            const targetUnitName = getUnitNameByInstructionId(conn.targetId!)!;
            const m = new Map<string, string>();
            m.set(targetUnitName, conn.variableName);
            dataFlows.add({
              source: getUnitNameByInstructionId(conn.sourceId!)!,
              targetsWithVars: m,
              net: "net1",
            });
          }
        }
      }
      return dataFlows;
    },
    [functions, dataFlowConnections],
  );

  const getRelatedUnitLabels = useCallback(
    (instructionId: number | null, dataFlowId: string | null): Set<string> => {
      const relatedLabels = new Set<string>();

      if (instructionId !== null) {
        for (const func of functions) {
          for (const instr of func.instructions) {
            if (instr.pID === instructionId) {
              relatedLabels.add(func.component);
              instr.sendsOutputsToPIDs.forEach((targetPID) => {
                for (const f of functions) {
                  for (const i of f.instructions) {
                    if (i.pID === targetPID) {
                      relatedLabels.add(f.component);
                    }
                  }
                }
              });
              instr.receiveInputsFromPIDs.forEach((sourcePID) => {
                for (const f of functions) {
                  for (const i of f.instructions) {
                    if (i.pID === sourcePID) {
                      relatedLabels.add(f.component);
                    }
                  }
                }
              });
              break;
            }
          }
        }
      } else if (dataFlowId !== null) {
        const relatedInstructions = getRelatedInstructions(dataFlowId);
        for (const instrId of relatedInstructions) {
          for (const func of functions) {
            for (const instr of func.instructions) {
              if (instr.pID === instrId) {
                relatedLabels.add(func.component);
              }
            }
          }
        }
      }

      return relatedLabels;
    },
    [functions, getRelatedInstructions],
  );

  useEffect(() => {
    const filteredFunctions = functions.filter((f) => {
      const hasUnit = enabledUnits.has(f.component);
      const hasFunction = enabledFunctions.has(f.label);
      return filterOperator === "AND"
        ? hasUnit && hasFunction
        : hasUnit || hasFunction;
    });
    setFilteredFunctions(filteredFunctions);

    if (selectedInstructionId !== null) {
      const isInstructionInFilteredFunctions = filteredFunctions.some((func) =>
        func.instructions.some((instr) => instr.pID === selectedInstructionId),
      );
      if (!isInstructionInFilteredFunctions) {
        setSelectedInstructionId(null);
      }
    }

    if (selectedDataFlowId !== null) {
      const [sourceId, targetId] = selectedDataFlowId
        .split(":")
        .map((id) => (id === "null" ? null : parseInt(id)));

      const isSourceInFilteredFunctions =
        sourceId === null ||
        filteredFunctions.some((func) =>
          func.instructions.some((instr) => instr.pID === sourceId),
        );
      const isTargetInFilteredFunctions =
        targetId === null ||
        filteredFunctions.some((func) =>
          func.instructions.some((instr) => instr.pID === targetId),
        );

      if (!isSourceInFilteredFunctions && !isTargetInFilteredFunctions) {
        setSelectedDataFlowId(null);
      }
    }

    const deepCopyUnit = (u: Unit): Unit => ({
      ...u,
      functions: u.functions ? [...u.functions] : null,
      subunits: u.subunits ? u.subunits.map((su) => deepCopyUnit(su)) : null,
    });

    const filterUnitFunctions = (u: Unit) => {
      if (u.functions) {
        u.functions = u.functions.filter((f) => {
          const hasUnit = enabledUnits.has(f.component);
          const hasFunction = enabledFunctions.has(f.label);
          return filterOperator === "AND"
            ? hasUnit && hasFunction
            : hasUnit || hasFunction;
        });
      }
      if (u.subunits)
        u.subunits.forEach((subu) => {
          filterUnitFunctions(subu);
        });
    };

    let filteredUnits = units;

    filteredUnits = filteredUnits.map((u) => deepCopyUnit(u));
    filteredUnits.forEach((u) => {
      filterUnitFunctions(u);
    });
    setFilteredUnits(filteredUnits);
  }, [
    functions,
    units,
    enabledUnits,
    enabledFunctions,
    filterOperator,
    selectedInstructionId,
    selectedDataFlowId,
  ]);

  const handleLayoutComplete = useCallback(
    (newContainerHeight: number, newTopPadding: number) => {
      setContainerHeight(newContainerHeight);
      setTopPadding(newTopPadding);
    },
    [],
  );

  const parseProcessDataLocal = useCallback(
    (
      timelinesResponse: ProcessTimelines<number>,
      processResponse: ProcessData,
    ) => {
      const {
        functions: functionsArray,
        dataFlowConnections: connections,
        units,
      } = parseProcessData(timelinesResponse, processResponse);

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
      setUnits(units);
      setTimelineConfig({ minTime, maxTime });
      setDataFlowConnections(connections);

      const allUnits = new Set(functionsArray.map((f) => f.component));
      setEnabledUnits(allUnits);

      const allFunctions = new Set(functionsArray.map((f) => f.label));
      setEnabledFunctions(allFunctions);
    },
    [],
  );

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
      <div className="filter-section">
        <div className="filter-container">
          <div className="buttons">
            <div className="units-section">
              <div className="unit-buttons">
                {Array.from(new Set(functions.map((f) => f.component))).map(
                  (component) => {
                    if (!selectedColorsRef.current.get(component)) return null;
                    return (
                      <UnitLabel
                        key={component}
                        componentName={component}
                        color={
                          COMPONENT_COLORS[
                            selectedColorsRef.current.get(component)!
                          ]
                        }
                        enabled={enabledUnits.has(component)}
                        onToggle={handleUnitToggle}
                      />
                    );
                  },
                )}
              </div>
            </div>
            <div className="filter-buttons">
              <button
                type="button"
                className={"filter-button functions-toggle-button"}
                onClick={handleToggleAllFunctions}
              >
                {enabledFunctions.size ===
                  new Set(functions.map((f) => f.label)).size &&
                enabledFunctions.size > 0
                  ? "Deselect all functions"
                  : "Select all functions"}
              </button>
              <button
                type="button"
                className={"filter-button units-toggle-button"}
                onClick={handleToggleAllUnits}
              >
                {enabledUnits.size ===
                  new Set(functions.map((f) => f.component)).size &&
                enabledUnits.size > 0
                  ? "Deselect all units"
                  : "Select all units"}
              </button>
              <button
                type="button"
                className="filter-button operator-toggle-button"
                onClick={() =>
                  setFilterOperator(filterOperator === "AND" ? "OR" : "AND")
                }
              >
                Filter operator: {filterOperator}
              </button>
              <button
                type="button"
                className="filter-button filter-toggle-button"
                onClick={() => setShowIntermediateView(!showIntermediateView)}
              >
                {showIntermediateView ? "Hide" : "Show"} graph filters
              </button>
            </div>
          </div>
          {showIntermediateView && (
            <SplitPane orientation="vertical" initialSplitPercentage={50}>
              <ClickableIntermediateView
                functionToUnitMapping={
                  new Map(functions.map((f) => [f.label, f.component]))
                }
                unitColors={selectedColorsRef.current}
                enabledFunctions={enabledFunctions}
                onToggle={handleFunctionToggle}
                highlightedFunctions={getRelatedFunctionLabels(
                  selectedInstructionId,
                  selectedDataFlowId,
                )}
                highlightedDataFlows={getRelatedDataFlowVariables(
                  selectedInstructionId,
                  selectedDataFlowId,
                )}
                selectedInstructionId={selectedInstructionId}
                selectedDataFlowId={selectedDataFlowId}
                onClearSelection={handleClearSelection}
              />
              <ClickableMicroarchitectureView
                unitColors={selectedColorsRef.current}
                enabledUnits={enabledUnits}
                highligthedUnits={getRelatedUnitLabels(
                  selectedInstructionId,
                  selectedDataFlowId,
                )}
                highlightedDataFlows={getRelatedDataFlowVariablesForMicroarchitectureView(
                  selectedInstructionId,
                  selectedDataFlowId,
                )}
                onToggle={handleUnitToggle}
                selectedInstructionId={selectedInstructionId}
                selectedDataFlowId={selectedDataFlowId}
                onClearSelection={handleClearSelection}
              />
            </SplitPane>
          )}
        </div>
      </div>

      {filteredFunctions.length === 0 ? (
        <div
          className="empty-state-container"
          style={{ padding: "1rem 2rem 1rem 2rem" }}
        >
          <div className="alert alert-info">
            No functions to display. Adjust filters to see the timeline.
          </div>
        </div>
      ) : (
        <div className="process-timelines-2-vertical">
          <div className="vertical-time-axis">
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
          </div>

          <div className="diagram-split-view">
            <SplitPane minWidthLeft={15} minWidthRight={15}>
              <FunctionTimeline
                functions={filteredFunctions}
                timelineConfig={timelineConfig}
                dataFlowConnections={dataFlowConnections}
                getComponentColor={getComponentColor}
                onLayoutComplete={handleLayoutComplete}
                selectedInstructionId={selectedInstructionId}
                selectedDataFlowId={selectedDataFlowId}
                onInstructionSelect={handleInstructionSelect}
                onDataFlowSelect={handleDataFlowSelect}
                getRelatedDataFlows={getRelatedDataFlows}
                getRelatedInstructions={getRelatedInstructions}
                onClearSelection={handleClearSelection}
              />
              <UnitTimeline
                functions={filteredFunctions}
                units={filteredUnits}
                timelineConfig={timelineConfig}
                topPadding={topPadding}
                containerHeight={containerHeight}
                getComponentColor={getComponentColor}
                dataFlowConnections={dataFlowConnections}
                selectedInstructionId={selectedInstructionId}
                selectedDataFlowId={selectedDataFlowId}
                onInstructionSelect={handleInstructionSelect}
                onDataFlowSelect={handleDataFlowSelect}
                getRelatedDataFlows={getRelatedDataFlows}
                getRelatedInstructions={getRelatedInstructions}
                onClearSelection={handleClearSelection}
              />
            </SplitPane>
          </div>
        </div>
      )}
      <div className={"p-3"}>Filtered Functions</div>
      <JsonView
        style={{ gap: "1rem", padding: "1rem 1rem 1rem 3rem" }}
        value={filteredFunctions}
        collapsed={1}
        shortenTextAfterLength={120}
      />
      <div className={"p-3"}>Data Flow Connections</div>
      <JsonView
        style={{ gap: "1rem", padding: "1rem 1rem 1rem 3rem" }}
        value={dataFlowConnections}
        collapsed={1}
        shortenTextAfterLength={120}
      />
    </div>
  );
};
