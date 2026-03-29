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
import { ClickableIntermediateView } from "./ProcessTimeline2/ClickableIntermediateView";
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
    console.log(label);
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

  useEffect(() => {
    const filteredFunctions = functions.filter(
      (f) => enabledUnits.has(f.component) && enabledFunctions.has(f.label),
    );
    setFilteredFunctions(filteredFunctions);

    const deepCopyUnit = (u: Unit): Unit => ({
      ...u,
      functions: u.functions ? [...u.functions] : null,
      subunits: u.subunits ? u.subunits.map((su) => deepCopyUnit(su)) : null,
    });

    const filterUnitFunctions = (u: Unit) => {
      if (u.functions)
        u.functions = u.functions.filter((f) => enabledFunctions.has(f.label));
      if (u.subunits)
        u.subunits.forEach((subu) => {
          filterUnitFunctions(subu);
        });
    };

    const filteredUnits = units
      .filter((u) => enabledUnits.has(u.name))
      .map((u) => deepCopyUnit(u));
    filteredUnits.forEach((u) => {
      filterUnitFunctions(u);
    });
    setFilteredUnits(filteredUnits);
  }, [functions, units, enabledUnits, enabledFunctions]);

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
            <button
              type="button"
              className="filter-toggle-button"
              onClick={() => setShowIntermediateView(!showIntermediateView)}
            >
              {showIntermediateView ? "Hide" : "Show"} Functions Filter
            </button>
          </div>
          {showIntermediateView && (
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
          )}
        </div>
      </div>

      <div className="process-timelines-2-vertical">
        <div className="vertical-time-axis">
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
        </div>

        <div className="diagram-split-view">
          <SplitPane
            initialSplitPercentage={70}
            minWidthLeft={15}
            minWidthRight={15}
          >
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
      <div>Filtered Functions</div>
      <JsonView
        style={{ gap: "2rem", padding: "3rem 3rem 3rem 4rem" }}
        value={filteredFunctions}
        collapsed={1}
        shortenTextAfterLength={120}
      />
      <div>Data Flow Connections</div>
      <JsonView
        style={{ gap: "2rem", padding: "3rem 3rem 3rem 4rem" }}
        value={dataFlowConnections}
        collapsed={1}
        shortenTextAfterLength={120}
      />
    </div>
  );
};
