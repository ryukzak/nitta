import React, {FC, useCallback, useEffect, useLayoutEffect, useState} from 'react';
import {ProcessFunction, instructionPositionsEqual, type DataFlowConnection} from '../utils/ProcessTimeline2';
import { Color } from '../../utils/color';
import { FunctionRectangle } from './FunctionRectangle';
import { DataFlowOverlay } from './DataFlows';
import { LegendItem } from './LegendItem';
import { type InstructionPosition } from '../utils/ArrowWithLabel';
import './UnitTimeline.scss';

interface TimelinePerUnitProps {
  functions: ProcessFunction[];
  timelineConfig: {
    minTime: number;
    maxTime: number;
  };
  rowHeight: number;
  topPadding: number;
  containerHeight: number;
  getComponentColor: (component: string) => Color;
  dataFlowConnections: DataFlowConnection[];
  onUnitToggle?: (unitName: string) => void;
  enabledUnits?: Set<string>;
  // instructionPositions?: Map<number, InstructionPosition>;
  // mostLeftFreeSpacesInColumnsPerRows?: Map<number, Map<number, number>>;
}

const calculateLeftPosition = (
  func: ProcessFunction,
  mostLeftFreeSpaces: Map<number, Map<number, number>>,
  headerHeight: number = 0,
  rowHeight: number = 70
): number => {
  let leftPosition = 0;

  for (
    let i = Math.max(
      func.startTime - Math.ceil(headerHeight / rowHeight),
      -1,
    );
    i <= func.endTime;
    i++
  ) {
    const rowSpaces = mostLeftFreeSpaces.get(i);
    if (rowSpaces) {
      const prevColSpace = rowSpaces.get(func.column - 1);
      if (prevColSpace !== undefined) {
        leftPosition = Math.max(leftPosition, prevColSpace);
      }
    }
  }

  return leftPosition;
};

export const UnitTimeline: FC<TimelinePerUnitProps> = ({
  functions,
  timelineConfig,
  rowHeight,
  topPadding,
  containerHeight,
  getComponentColor,
  dataFlowConnections,
  onUnitToggle,
  enabledUnits = new Set(),
  // instructionPositions = new Map(),
  // mostLeftFreeSpacesInColumnsPerRows,
}) => {
  const [instructionPositions, setInstructionPositions] = useState<
    Map<number, InstructionPosition>
  >(new Map());
  const containerRef = React.useRef<HTMLDivElement>(null);
  console.log(functions);
  // Group functions by component (unit)
  const unitsMap = new Map<string, ProcessFunction[]>();
  functions.forEach((f) => {
    if (!unitsMap.has(f.component)) {
      unitsMap.set(f.component, []);
    }
    unitsMap.get(f.component)?.push(f);
  });

  const units = Array.from(unitsMap.entries());

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
            column: units.map(([key]) => key).indexOf(func.component)
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

  // Create a default mostLeftFreeSpaces map with left margin for per-unit view
  const defaultMostLeftFreeSpaces = new Map<number, Map<number, number>>();
  for (let i = timelineConfig.minTime - 1; i <= timelineConfig.maxTime + 1; i++) {
    const columnsMap = new Map<number, number>();
    columnsMap.set(-1, 0);
    columnsMap.set(0, 20); // left margin for per-unit view
    defaultMostLeftFreeSpaces.set(i, columnsMap);
  }

  const mostLeftFreeSpaces = new Map<number, Map<number, number>>();
  for (let i = timelineConfig.minTime - 1; i <= timelineConfig.maxTime + 1; i++) {
    const columnsMap = new Map<number, number>();
    columnsMap.set(-1, 0);
    columnsMap.set(0, 20); // left margin for per-unit view
    mostLeftFreeSpaces.set(i, columnsMap);
  }

  useLayoutEffect(() => {
    if (containerHeight > 0) calculateInstructionPositions();
  }, [containerHeight, calculateInstructionPositions]);

  useEffect(() => {
    // Measure the widest function rectangle per unit and apply that width to all rectangles in that unit
    const unitRows = containerRef.current?.querySelectorAll('.unit-row');
    if (!unitRows || unitRows.length === 0) return;

    unitRows.forEach((unitRow) => {
      const functionRectangles = unitRow.querySelectorAll('.function-rectangle');
      if (functionRectangles.length === 0) return;

      let maxWidth = 0;
      functionRectangles.forEach((rect) => {
        const width = rect.getBoundingClientRect().width;
        maxWidth = Math.max(maxWidth, width);
      });

      // Apply the max width to all function rectangles, headers, and instructions containers in this unit
      if (maxWidth > 0) {
        functionRectangles.forEach((rect) => {
          (rect as HTMLElement).style.width = `${maxWidth}px`;

          const header = rect.querySelector('.function-header') as HTMLElement;
          if (header) {
            header.style.width = `${maxWidth}px`;
          }

          const instructionsContainer = rect.querySelector('.instructions-container') as HTMLElement;
          if (instructionsContainer) {
            instructionsContainer.style.width = `${maxWidth}px`;
          }
        });
      }
    });
  }, [functions]);

  useEffect(() => {
    // Adjust spacing between unit columns based on arrow label widths
    const unitRows = containerRef.current?.querySelectorAll('.unit-row');
    if (!unitRows || unitRows.length === 0) return;

    // Get all arrow label groups from the SVG overlay
    const arrowLabels = containerRef.current?.querySelectorAll('.arrow-label-group');
    if (!arrowLabels || arrowLabels.length === 0) return;

    // Map to store max label width per row
    const labelWidthsByRow = new Map<number, number>();

    arrowLabels.forEach((labelGroup) => {
      const rect = labelGroup.getBoundingClientRect();
      const containerRect = containerRef.current!.getBoundingClientRect();
      const relativeY = rect.top - containerRect.top + containerRef.current!.scrollTop;

      // Round to nearest row to group labels in same row
      const rowKey = Math.round(relativeY / rowHeight) * rowHeight;
      const labelWidth = rect.width;

      const currentMax = labelWidthsByRow.get(rowKey) || 0;
      labelWidthsByRow.set(rowKey, Math.max(currentMax, labelWidth));
    });

    // Apply spacing between unit columns based on the maximum label widths
    if (labelWidthsByRow.size > 0) {
      const maxLabelWidth = Math.max(...Array.from(labelWidthsByRow.values()));
      const requiredGap = maxLabelWidth + 20; // Add padding

      const unitsContainer = containerRef.current?.querySelector('.units-container') as HTMLElement;
      if (unitsContainer) {
        unitsContainer.style.columnGap = `${requiredGap}px`;
      }

      // Also set gap on unit-columns
      const unitColumns = containerRef.current?.querySelectorAll('.unit-column');
      unitColumns?.forEach((col) => {
        (col as HTMLElement).style.marginRight = `${requiredGap}px`;
      });

      // Update instruction positions after spacing adjustment
      calculateInstructionPositions();
    }
  }, [functions, rowHeight, calculateInstructionPositions]);

  return (
    <div ref={containerRef} className="timeline-per-unit">
      <DataFlowOverlay
                    topPadding={topPadding}
                    timelineConfig={timelineConfig}
                    rowHeight={rowHeight}
                    dataFlowConnections={dataFlowConnections}
                    instructionPositions={instructionPositions}
                  />
      <div className="units-container" style={{ minHeight: containerHeight }}>
        {units.map(([unitName, unitFunctions]) => {
          const unitColor = getComponentColor(unitName);

          // Calculate the required width for the unit column to fit all function rectangles
          let maxWidth = 0;
          unitFunctions.forEach(f => {
            const leftPos = calculateLeftPosition(f, mostLeftFreeSpaces, 0, rowHeight);
            const rightEdge = f.width;
            maxWidth = Math.max(maxWidth, rightEdge);
          });

          return (
            <div key={unitName} className="unit-row">
              <div className="unit-column">
                <LegendItem
                  componentName={unitName}
                  color={unitColor}
                  enabled={enabledUnits.has(unitName)}
                  onToggle={onUnitToggle}
                />
                <div className="unit-timeline-track" style={maxWidth > 0 ? { width: `${maxWidth}px` } : {}}>
                  {/*{dataFlowConnections.length > 0 && instructionPositions.size > 0 && (*/}

                  {/*)}*/}
                   {unitFunctions.map(f => (
                     <FunctionRectangle
                       key={f.pID}
                       func={f}
                       bgColor={unitColor}
                       headerHeight={0}
                       rowHeight={rowHeight}
                       topPadding={topPadding}
                       timelineConfig={timelineConfig}
                       mostLeftFreeSpacesInColumnsPerRows={mostLeftFreeSpaces}
                       headerMode="inside"
                     />
                   ))}
                </div>
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );
};
