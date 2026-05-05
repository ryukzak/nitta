import React, {
  type FC,
  useCallback,
  useEffect,
  useLayoutEffect,
  useMemo,
  useRef,
  useState,
} from "react";
import type { Color } from "../../utils/color";
import type { InstructionPosition } from "../utils/ArrowWithLabel";
import {
  assignInputOutputPositions,
  calculateInstructionPositionsFromDOM,
  createContainerClickHandler,
  createZeroMap,
  type DataFlowConnection,
  estimateArrowTextWidth,
  instructionPositionsEqual,
  LabelPosition,
  type ProcessFunction,
  ROW_HEIGHT,
  type Unit,
} from "../utils/ProcessTimeline2";
import { ColorLabelButton } from "./ColorLabelButton";
import { DataFlowOverlay } from "./DataFlows";
import { FunctionRectangle } from "./FunctionRectangle";
import { useWheelScale } from "./hooks/useWheelScale";

import "./UnitTimeline.scss";
import "components/ProcessTimeline2/TimelineContainer.scss";

interface Props {
  functions: ProcessFunction[];
  units: Unit[];
  timelineConfig: { minTime: number; maxTime: number };
  topPadding: number;
  containerHeight: number;
  getComponentColor: (component: string) => Color;
  dataFlowConnections: DataFlowConnection[];

  selectedInstructionId: number | null;
  selectedDataFlowId: string | null;

  onInstructionSelect: (id: number) => void;
  onDataFlowSelect: (id: string) => void;

  getRelatedDataFlows: (id: number) => string[];
  getRelatedInstructions: (id: string) => number[];

  onClearSelection: () => void;
  scale: number;
  onScaleChange: (delta: number) => void;
}

export const UnitTimeline: FC<Props> = ({
  functions,
  units,
  timelineConfig,
  topPadding,
  containerHeight,
  getComponentColor,
  dataFlowConnections,
  selectedInstructionId,
  selectedDataFlowId,
  onInstructionSelect,
  onDataFlowSelect,
  getRelatedDataFlows,
  getRelatedInstructions,
  onClearSelection,
  scale,
  onScaleChange,
}) => {
  const containerRef = useRef<HTMLDivElement>(null);

  const [instructionPositions, setInstructionPositions] = useState<
    Map<number, InstructionPosition>
  >(new Map());

  const renderUnits = units
    .filter((u) => {
      if (!u.subunits && (!u.functions || u.functions.length === 0))
        return false;

      if (u.subunits) {
        let allSubUnitsAreEmpty = true;
        u.subunits.forEach((su) => {
          if (su.functions && su.functions.length !== 0)
            allSubUnitsAreEmpty = false;
        });
        return !allSubUnitsAreEmpty;
      }
      return true;
    })
    .map((u) => ({
      unit: u,
      subunits: u.subunits ?? undefined,
    }));

  const flatUnits: { unit: Unit; parent?: Unit }[] = [];

  renderUnits.forEach(({ unit, subunits }) => {
    if (unit.functions) flatUnits.push({ unit });

    subunits?.forEach((su) => {
      flatUnits.push({ unit: su, parent: unit });
    });
  });

  const calculateInstructionPositions = useCallback(() => {
    const container = containerRef.current;
    if (!container) return;

    const map = calculateInstructionPositionsFromDOM(
      container,
      functions,
      getComponentColor,
      (func) => {
        const idx = flatUnits.findIndex((u) =>
          u.unit.functions?.some((f) => f.label === func.label),
        );
        return idx < 0 ? 0 : idx;
      },
      scale,
    );

    setInstructionPositions((prev) =>
      instructionPositionsEqual(prev, map) ? prev : map,
    );
  }, [functions, getComponentColor, flatUnits.findIndex, scale]);

  useLayoutEffect(() => {
    if (containerHeight > 0) calculateInstructionPositions();
  }, [containerHeight, calculateInstructionPositions]);

  useEffect(() => {
    const container = containerRef.current;
    if (!container) return;

    assignInputOutputPositions(functions);

    const prevBorders = createZeroMap(
      timelineConfig.minTime,
      timelineConfig.maxTime,
    );

    const parentLeftMap = new Map<string, number>();
    const parentMaxTimeMap = new Map<string, number>();

    flatUnits.forEach(({ unit, parent }) => {
      const funcs = unit.functions ?? [];

      const leftWidths = createZeroMap(
        timelineConfig.minTime,
        timelineConfig.maxTime,
      );
      const rightWidths = createZeroMap(
        timelineConfig.minTime,
        timelineConfig.maxTime,
      );

      let maxTime = -1;

      for (const f of funcs) {
        for (const i of f.instructions) {
          i.inputPositions.forEach((pos, label) => {
            const w = estimateArrowTextWidth(label);
            if (pos === LabelPosition.Left) leftWidths.set(i.startTime, w);
            else rightWidths.set(i.startTime, w);
          });

          i.outputPositions.forEach((pos, label) => {
            const w = estimateArrowTextWidth(label);
            if (pos === LabelPosition.Left) leftWidths.set(i.startTime, w);
            else rightWidths.set(i.startTime, w);
          });
        }

        maxTime = Math.max(maxTime, f.endTime);
      }

      let globalLeft = 0;

      for (let t = timelineConfig.minTime; t <= timelineConfig.maxTime; t++) {
        globalLeft = Math.max(
          globalLeft,
          prevBorders.get(t)! + leftWidths.get(t)!,
        );
      }

      const selector = parent
        ? `[data-subunit="${parent.name + unit.name}"]`
        : `[data-unit="${unit.name}"]`;

      const elem = container.querySelector(selector) as HTMLElement;
      if (!elem) return;

      if (!parent) {
        // unit
        elem.style.left = `${globalLeft}px`;
        parentLeftMap.set(unit.name, globalLeft);
        elem.style.height = `${(maxTime - timelineConfig.minTime + 2) * ROW_HEIGHT}px`;
      } else {
        // subunit
        const parentColumn = container.querySelector(
          `[data-unit=${parent.name}]`,
        ) as HTMLElement;
        if (!parentLeftMap.has(parent.name)) {
          parentLeftMap.set(parent.name, globalLeft);
          parentMaxTimeMap.set(parent.name, -1);
          parentColumn.style.left = `${globalLeft}px`;
        }
        if (parentMaxTimeMap.get(parent.name)! < maxTime) {
          parentColumn.style.height = `${(maxTime - timelineConfig.minTime + 2) * ROW_HEIGHT}px`;
          parentMaxTimeMap.set(parent.name, maxTime);
        }

        const parentLeft = parentLeftMap.get(parent.name) ?? 0;

        elem.style.left = `${globalLeft - parentLeft}px`;
        parentColumn.style.width = `${globalLeft - parentLeft + elem.getBoundingClientRect().width / scale}px`;

        const header = elem.querySelector(`.subunit-header`) as HTMLElement;
        elem.style.height = `${(maxTime - timelineConfig.minTime + 2) * ROW_HEIGHT - header.getBoundingClientRect().height / scale}px`;
      }

      const width = elem.getBoundingClientRect().width / scale;

      for (let t = timelineConfig.minTime; t <= timelineConfig.maxTime; t++) {
        prevBorders.set(t, globalLeft + width + rightWidths.get(t)!);
      }
    });

    calculateInstructionPositions();
  }, [
    functions,
    calculateInstructionPositions,
    timelineConfig.minTime,
    timelineConfig.maxTime,
    flatUnits.forEach,
    scale,
  ]);

  const renderFunctions = (funcs: ProcessFunction[], color: Color) =>
    funcs.map((f) => (
      <FunctionRectangle
        key={f.pID}
        func={f}
        bgColor={color}
        topPadding={topPadding}
        timelineConfig={timelineConfig}
        headerMode="inside"
        selectedInstructionId={selectedInstructionId}
        selectedDataFlowId={selectedDataFlowId}
        onInstructionSelect={onInstructionSelect}
        getRelatedDataFlows={getRelatedDataFlows}
        getRelatedInstructions={getRelatedInstructions}
      />
    ));

  useLayoutEffect(() => {
    const container = containerRef.current;
    if (!container) return;

    const tracks = container.querySelectorAll(".unit-timeline-track");

    tracks.forEach((track) => {
      const rects = track.querySelectorAll(".function-rectangle");

      let maxWidth = 0;

      rects.forEach((r) => {
        maxWidth = Math.max(
          maxWidth,
          (r as HTMLElement).getBoundingClientRect().width / scale,
        );
      });

      if (!maxWidth) return;

      (track as HTMLElement).style.width = `${maxWidth}px`;

      const column = track.closest(
        ".unit-column, .subunit-column",
      ) as HTMLElement;

      if (column) {
        column.style.width = `${maxWidth}px`;
      }

      let headerOffset = 0;
      if (column?.classList.contains("subunit-column")) {
        const header = column.querySelector(".subunit-header") as HTMLElement;
        if (header) {
          headerOffset = header.getBoundingClientRect().height / scale;
        }
      }

      rects.forEach((r) => {
        const elem = r as HTMLElement;
        elem.style.width = `${maxWidth}px`;

        if (headerOffset > 0) {
          elem.style.transform = `translateY(-${headerOffset}px)`;
        } else {
          elem.style.transform = "";
        }

        const functionHeader = elem.querySelector(
          ".function-header",
        ) as HTMLElement;
        if (functionHeader) {
          functionHeader.style.width = `${maxWidth}px`;
        }

        const instructionsContainer = elem.querySelector(
          ".instructions-container",
        ) as HTMLElement;
        if (instructionsContainer) {
          instructionsContainer.style.width = `${maxWidth}px`;
        }
      });
    });
  }, [scale]);

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Escape") {
      onClearSelection();
    }
  };

  useWheelScale(containerRef, onScaleChange);

  return (
    <section
      ref={containerRef}
      className="timeline-container unit-timeline"
      onClick={createContainerClickHandler(containerRef, onClearSelection)}
      onKeyDown={handleKeyDown}
      tabIndex={-1}
      aria-label="Unit timeline"
    >
      <div style={{ minHeight: containerHeight * scale }}>
        <div
          style={{ transform: `scale(${scale})`, transformOrigin: "0px 0px" }}
        >
          <DataFlowOverlay
            topPadding={topPadding}
            timelineConfig={timelineConfig}
            dataFlowConnections={dataFlowConnections}
            instructionPositions={instructionPositions}
            selectedInstructionId={selectedInstructionId}
            selectedDataFlowId={selectedDataFlowId}
            onDataFlowSelect={onDataFlowSelect}
            getRelatedInstructions={getRelatedInstructions}
          />
          <div className="units-container">
            {renderUnits.map(({ unit, subunits }) => {
              const color = getComponentColor(unit.name);

              return (
                <div
                  key={unit.name}
                  className="unit-column"
                  data-unit={unit.name}
                >
                  <div className="unit-header">
                    <ColorLabelButton
                      componentName={unit.name}
                      color={color}
                      enabled
                    />
                  </div>

                  {/* units */}
                  {unit.functions && (
                    <div className="unit-timeline-track">
                      {renderFunctions(unit.functions, color)}
                    </div>
                  )}

                  {/* subunits */}
                  {subunits?.map((su) => (
                    <div
                      key={su.name}
                      className="subunit-column"
                      data-subunit={unit.name + su.name}
                    >
                      <div className="subunit-header">
                        <ColorLabelButton
                          componentName={su.name}
                          color={color}
                          enabled
                        />
                      </div>

                      <div
                        className="unit-timeline-track"
                        data-subunit={unit.name + su.name}
                      >
                        {renderFunctions(su.functions ?? [], color)}
                      </div>
                    </div>
                  ))}
                </div>
              );
            })}
          </div>
        </div>
      </div>
    </section>
  );
};
