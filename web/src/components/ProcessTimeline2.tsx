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
import { SplitPane } from "./utils/SplitPane";
import { COMPONENT_COLORS, Color, fadeColor } from "../utils/color";
import { JsonView } from "./JsonView";
import {
  type InstructionPosition,
} from "./utils/ArrowWithLabel";
import {
  type DataFlowConnection,
  type ProcessFunction,
  parseProcessData,
} from "./utils/ProcessTimeline2";

const ROW_HEIGHT = 70;
const COLUMN_MARGIN = 20;

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
  const [
    mostLeftFreeSpacesInColumnsPerRows,
    setMostLeftFreeSpacesInColumnsPerRows,
  ] = useState<Map<number, Map<number, number>>>(new Map());
  const selectedColorsRef = React.useRef<Map<string, string>>(new Map());
  const [topPadding, setTopPadding] = useState(0);

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

  const handleLayoutComplete = useCallback(
    (
      layoutFunctions: ProcessFunction[],
      newContainerHeight: number,
      newTopPadding: number,
      newMostLeftFreeSpaces: Map<number, Map<number, number>>,
      newInstructionPositions: Map<number, InstructionPosition>,
    ) => {
      setFunctions(layoutFunctions);
      setContainerHeight(newContainerHeight);
      setTopPadding(newTopPadding);
      setMostLeftFreeSpacesInColumnsPerRows(newMostLeftFreeSpaces);
      setInstructionPositions(newInstructionPositions);
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
      setContainerHeight((maxTime - minTime + 1) * ROW_HEIGHT + 100);
      setDataFlowConnections(connections);
      setMostLeftFreeSpacesInColumnsPerRows(initialMostLeftSpaces);
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
          <SplitPane initialSplitPercentage={70} minWidthLeft={15} minWidthRight={15}>
            <FunctionTimeline
              functions={functions}
              timelineConfig={timelineConfig}
              dataFlowConnections={dataFlowConnections}
              getComponentColor={getComponentColor}
              onLayoutComplete={handleLayoutComplete}
            />

            <UnitTimeline
              functions={functions}
              timelineConfig={timelineConfig}
              rowHeight={ROW_HEIGHT}
              topPadding={topPadding}
              containerHeight={containerHeight}
              getComponentColor={getComponentColor}
              dataFlowConnections={dataFlowConnections}
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
