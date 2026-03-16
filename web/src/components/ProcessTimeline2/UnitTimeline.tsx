import React, { FC } from 'react';
import { ProcessFunction, type DataFlowConnection } from '../utils/ProcessTimeline2';
import { Color } from '../../utils/color';
import { FunctionRectangle } from './FunctionRectangle';
import { DataFlowOverlay } from './DataFlows';
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
  // dataFlowConnections?: DataFlowConnection[];
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
  // dataFlowConnections = [],
  // instructionPositions = new Map(),
  // mostLeftFreeSpacesInColumnsPerRows,
}) => {
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

  return (
    <div className="timeline-per-unit">
      <DataFlowOverlay
                    topPadding={topPadding}
                    timelineConfig={timelineConfig}
                    rowHeight={rowHeight}
                  />
      <div className="units-container" style={{ minHeight: containerHeight }}>
        {units.map(([unitName, unitFunctions]) => {
          const unitColor = getComponentColor(unitName);

          // Calculate the required width for the unit column to fit all function rectangles
          let maxWidth = 0;
          unitFunctions.forEach(f => {
            const leftPos = calculateLeftPosition(f, mostLeftFreeSpaces, 0, rowHeight);
            const rightEdge = leftPos + f.width;
            maxWidth = Math.max(maxWidth, rightEdge);
          });

          return (
            <div key={unitName} className="unit-row">
              <div className="unit-column">
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
