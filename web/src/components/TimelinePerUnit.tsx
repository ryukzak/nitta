import React, { FC } from 'react';
import { ProcessFunction } from './utils/ProcessTimeline2';
import { Color } from '../utils/color';
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
}

export const TimelinePerUnit: FC<TimelinePerUnitProps> = ({
  functions,
  timelineConfig,
  rowHeight,
  topPadding,
  containerHeight,
  getComponentColor,
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

  return (
    <div className="timeline-per-unit">
      <div className="units-container" style={{ minHeight: containerHeight }}>
        {units.map(([unitName, unitFunctions]) => {
          const unitColor = getComponentColor(unitName);
          const bgTransparentColor = new Color({
            r: unitColor.obj.r,
            g: unitColor.obj.g,
            b: unitColor.obj.b,
          });
          bgTransparentColor.obj.a = 0x30 / 255;

          return (
            <div key={unitName} className="unit-row">
              <div className="unit-timeline-track">
                 {unitFunctions.map(f => (
                   <div
                     key={f.pID}
                     className="unit-function-block"
                     style={{
                       top: (f.startTime - timelineConfig.minTime) * rowHeight,
                       height: (f.endTime - f.startTime + 1) * rowHeight,
                       backgroundColor: bgTransparentColor.toHexString(),
                       borderColor: unitColor.toHexString(),
                     }}
                     title={`${f.label} (${f.startTime}-${f.endTime})`}
                   >
                     <div className="unit-block-label">{f.label}</div>
                   </div>
                 ))}
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );
};
