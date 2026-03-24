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
import { UnitTimeline } from "./ProcessTimeline2/UnitTimeline";
import { FunctionTimeline } from "./ProcessTimeline2/FunctionTimeline";
import { UnitLabel } from "./ProcessTimeline2/UnitLabel";
import { SplitPane } from "./utils/SplitPane";
import { COMPONENT_COLORS, Color } from "../utils/color";
import { JsonView } from "./JsonView";
import {
  type DataFlowConnection,
  type ProcessFunction,
  parseProcessData, COLUMN_MARGIN,
  ROW_HEIGHT,
} from "./utils/ProcessTimeline2";
import { ClickableIntermediateView } from "./ProcessTimeline2/ClickableIntermediateView";



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
  const selectedColorsRef = React.useRef<Map<string, string>>(new Map());
  const [topPadding, setTopPadding] = useState(0);
  const [enabledUnits, setEnabledUnits] = useState<Set<string>>(new Set());
  const [enabledFunctions, setEnabledFunctions] = useState<Set<string>>(new Set());
  const [filteredFunctions, setFilteredFunctions] = useState<ProcessFunction[]>([]);
  const [showIntermediateView, setShowIntermediateView] = useState(false);

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

  // Update filteredFunctions when functions or enabledUnits change
  useEffect(() => {
    const filtered = functions.filter(
      f => enabledUnits.has(f.component) && enabledFunctions.has(f.label));
    setFilteredFunctions(filtered);
  }, [functions, enabledUnits, enabledFunctions]);

  const handleLayoutComplete = useCallback(
    (
      newContainerHeight: number,
      newTopPadding: number,
    ) => {
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
      // setContainerHeight((maxTime - minTime + 1) * ROW_HEIGHT + 100);
      setDataFlowConnections(connections);

      // Initialize all units as enabled
      const allUnits = new Set(functionsArray.map(f => f.component));
      setEnabledUnits(allUnits);

      // Initialize all functions as enabled
      const allFunctions = new Set(functionsArray.map(f => f.label));
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
              {
                Array.from(
                  new Set(functions.map(f => f.component))
                ).map(
                  (component) => {
                    if (!selectedColorsRef.current.get(component)) return;
                    return <UnitLabel
                      key={component}
                      componentName={component}
                      color={COMPONENT_COLORS[selectedColorsRef.current.get(component)!]}
                      enabled={enabledUnits.has(component)}
                      onToggle={handleUnitToggle}
                    />
                  })
              }
            </div>
            <button
              className="filter-toggle-button"
              onClick={() => setShowIntermediateView(!showIntermediateView)}
            >
              {showIntermediateView ? "Hide" : "Show"} Functions Filter
            </button>
          </div>
          {showIntermediateView && (
            <ClickableIntermediateView
              functionToUnitMapping={new Map(functions.map(f => [f.label, f.component]))}
              unitColors={selectedColorsRef.current}
              enabledFunctions={enabledFunctions}
              onToggle={handleFunctionToggle}
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
          <SplitPane initialSplitPercentage={70} minWidthLeft={15} minWidthRight={15}>
            <FunctionTimeline
              functions={filteredFunctions}
              timelineConfig={timelineConfig}
              dataFlowConnections={dataFlowConnections}
              getComponentColor={getComponentColor}
              onLayoutComplete={handleLayoutComplete}
            />
            <UnitTimeline
              functions={filteredFunctions}
              timelineConfig={timelineConfig}
              topPadding={topPadding}
              containerHeight={containerHeight}
              getComponentColor={getComponentColor}
              dataFlowConnections={dataFlowConnections}
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
