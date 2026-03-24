import React, {FC, useCallback, useEffect, useLayoutEffect, useState} from 'react';
import {
  ProcessFunction,
  instructionPositionsEqual,
  type DataFlowConnection,
  LabelPosition, estimateArrowTextWidth, ROW_HEIGHT,
  assignInputOutputPositions,
  calculateInstructionPositionsFromDOM,
  createZeroMap,
} from '../utils/ProcessTimeline2';
import { Color } from '../../utils/color';
import { FunctionRectangle } from './FunctionRectangle';
import { DataFlowOverlay } from './DataFlows';
import { UnitLabel } from './UnitLabel';
import { type InstructionPosition } from '../utils/ArrowWithLabel';
import './UnitTimeline.scss';
import "components/ProcessTimeline2/TimelineContainer.scss";

interface TimelinePerUnitProps {
  functions: ProcessFunction[];
  timelineConfig: {
    minTime: number;
    maxTime: number;
  };
  topPadding: number;
  containerHeight: number;
  getComponentColor: (component: string) => Color;
  dataFlowConnections: DataFlowConnection[];
}

export const UnitTimeline: FC<TimelinePerUnitProps> = ({
  functions,
  timelineConfig,
  topPadding,
  containerHeight,
  getComponentColor,
  dataFlowConnections
}) => {
  const [instructionPositions, setInstructionPositions] = useState<
    Map<number, InstructionPosition>
  >(new Map());
  const containerRef = React.useRef<HTMLDivElement>(null);

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
    const positionsMap = calculateInstructionPositionsFromDOM(
      container,
      functions,
      getComponentColor,
      (func) => units.map(([key]) => key).indexOf(func.component),
    );

    setInstructionPositions((prevPositions) => {
      if (instructionPositionsEqual(prevPositions, positionsMap)) {
        return prevPositions;
      }
      return positionsMap;
    });
  }, [functions, getComponentColor, units]);

  useLayoutEffect(() => {
    if (containerHeight > 0) calculateInstructionPositions();
  }, [containerHeight, calculateInstructionPositions]);

  useEffect(() => {
    // Measure the widest function rectangle per unit and apply that width to all rectangles in that unit
    const unitRows = containerRef.current?.querySelectorAll('.unit-column');
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

    const containerElem = containerRef.current;
    if (!containerElem) return;

    // assign input/output positions
    assignInputOutputPositions(functions);

    let prevColumnRigthBordersPerRows = createZeroMap(timelineConfig.minTime, timelineConfig.maxTime);

    units.forEach((ufs, index) => {

      let leftArrowLabelWidthsPerRows = createZeroMap(timelineConfig.minTime, timelineConfig.maxTime);
      let rightArrowLabelWidthPerRows = createZeroMap(timelineConfig.minTime, timelineConfig.maxTime);
      let unitMaxTime = -1;

      ufs[1].forEach((f) => {
        console.log("PROCESSING " + f.label)
        console.log("[" + f.startTime + "; " + f.endTime + "]")
        f.instructions.forEach((i) => {
          i.inputPositions.forEach((pos, label) => {
            if (pos === LabelPosition.Left) {
              leftArrowLabelWidthsPerRows.set(i.startTime, estimateArrowTextWidth(label));
            } else if (pos === LabelPosition.Right) {
              rightArrowLabelWidthPerRows.set(i.startTime, estimateArrowTextWidth(label));
            }
          })
          i.outputPositions.forEach((pos, label) => {
            if (pos === LabelPosition.Left) {
              leftArrowLabelWidthsPerRows.set(i.startTime, estimateArrowTextWidth(label));
            } else if (pos === LabelPosition.Right) {
              rightArrowLabelWidthPerRows.set(i.startTime, estimateArrowTextWidth(label));
            }
          })
          if (f.endTime > unitMaxTime)
            unitMaxTime = f.endTime;
        })
      })

      let unitLeftPosition = -1;

      for (let i: number = timelineConfig.minTime; i <= timelineConfig.maxTime; i++) {
        const l = prevColumnRigthBordersPerRows.get(i)! + leftArrowLabelWidthsPerRows.get(i)!;
        if (l > unitLeftPosition) {
          unitLeftPosition = l;
        }
      }

      console.log(prevColumnRigthBordersPerRows);
      console.log(leftArrowLabelWidthsPerRows);

      const unitColumnElem = containerElem.querySelector(`[data-unit=${ufs[0]}]`) as HTMLElement;
      const unitWidth = unitColumnElem.getBoundingClientRect().width;
      unitColumnElem.style.left = `${unitLeftPosition}px`;
      unitColumnElem.style.height = `${(unitMaxTime - timelineConfig.minTime + 2) * ROW_HEIGHT}px`;

      for (let i: number = timelineConfig.minTime; i <= timelineConfig.maxTime; i++) {
        prevColumnRigthBordersPerRows.set(i, unitLeftPosition + unitWidth + rightArrowLabelWidthPerRows.get(i)!);
      }
    })
      // Update instruction positions after spacing adjustment
      calculateInstructionPositions();
  }, [functions, dataFlowConnections, ROW_HEIGHT, calculateInstructionPositions]);

  return (
    <div ref={containerRef} className="timeline-container unit-timeline">
      <DataFlowOverlay
                    topPadding={topPadding}
                    timelineConfig={timelineConfig}
                    dataFlowConnections={dataFlowConnections}
                    instructionPositions={instructionPositions}
                  />
      <div className="units-container" style={{ minHeight: containerHeight }}>
        {units.map(([unitName, unitFunctions]) => {
          const unitColor = getComponentColor(unitName);

          // Calculate the required width for the unit column to fit all function rectangles
          let maxWidth = 0;
          unitFunctions.forEach(f => {
            const rightEdge = f.width;
            maxWidth = Math.max(maxWidth, rightEdge);
          });

          return (
              <div key={unitName} className="unit-column" data-unit={unitName}>
                <div className="unit-header">
                  <UnitLabel
                    componentName={unitName}
                    color={unitColor}
                    enabled={true}
                  />
                </div>
                <div className="unit-timeline-track" style={maxWidth > 0 ? { width: `${maxWidth}px` } : {}}>
                   {unitFunctions.map(f => (
                     <FunctionRectangle
                       key={f.pID}
                       func={f}
                       bgColor={unitColor}
                       topPadding={topPadding}
                       timelineConfig={timelineConfig}
                       headerMode="inside"
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
