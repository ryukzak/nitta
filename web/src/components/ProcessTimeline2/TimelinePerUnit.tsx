import React, { FC } from 'react';
import { ProcessFunction, type DataFlowConnection } from '../utils/ProcessTimeline2';
import { Color } from '../../utils/color';
import { FunctionRectangle } from './FunctionBlockComponents';
import { DataFlowOverlay } from './TimelineView';
import { type InstructionPosition } from '../utils/ArrowWithLabel';
import './TimelinePerUnit.scss';

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

export const TimelinePerUnit: FC<TimelinePerUnitProps> = ({
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
          return (
            <div key={unitName} className="unit-row">
              <div className="unit-timeline-track">
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
                   />
                 ))}
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );
};
