import React, {FC, useCallback, useEffect, useLayoutEffect, useState} from 'react';
import {
  ProcessFunction,
  instructionPositionsEqual,
  type DataFlowConnection,
  LabelPosition, estimateArrowTextWidth, ROW_HEIGHT
} from '../utils/ProcessTimeline2';
import { Color } from '../../utils/color';
import { FunctionRectangle } from './FunctionRectangle';
import { DataFlowOverlay } from './DataFlows';
import { LegendItem } from './LegendItem';
import { type InstructionPosition } from '../utils/ArrowWithLabel';
import './UnitTimeline.scss';
import "components/ProcessTimeline2/TimelineContainer.scss";

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
}

export const UnitTimeline: FC<TimelinePerUnitProps> = ({
  functions,
  timelineConfig,
  rowHeight,
  topPadding,
  containerHeight,
  getComponentColor,
  dataFlowConnections,
  onUnitToggle,
  enabledUnits = new Set()
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

    const getInstructionColumnByPID = (PID: number) => {
      for (const f of functions) {
        for (const i of f.instructions) {
          if (i.pID === PID) return f.column;
        }
      }
      return null;
    };

    const getArrowLabelPosition = (fromColumn: number, toColumn: number) => {
      return toColumn >= fromColumn ? LabelPosition.Right : LabelPosition.Left;
    }

    // assign input/output positions
    functions.forEach((f) => {
      f.instructions.forEach((i) => {
        i.inputs.forEach((inp) => {
          const targetInstrucionPID = i.receiveInputsFromPIDs.get(inp);
          let inputPosition;
          if (targetInstrucionPID === undefined || getInstructionColumnByPID(targetInstrucionPID) === null) inputPosition = LabelPosition.Left;
          else inputPosition = getArrowLabelPosition(f.column, getInstructionColumnByPID(targetInstrucionPID)!);
          i.inputPositions.set(inp, inputPosition)
        })
        i.outputs.forEach((outp) => {
          const targetInstrucionPID = i.sendsOutputsToPIDs.get(outp);
          let outputPosition;
          if (targetInstrucionPID === undefined || getInstructionColumnByPID(targetInstrucionPID) === null) outputPosition = LabelPosition.Right;
          else outputPosition = getArrowLabelPosition(f.column, getInstructionColumnByPID(targetInstrucionPID)!);
          i.outputPositions.set(outp, outputPosition)
        })
      });
    });

    const createZeroMap = (min: number = timelineConfig.minTime, max: number = timelineConfig.maxTime) =>
      new Map<number, number>(
        Array.from({length: max - min + 1}, (_, i) => [min + i, 0])
      );

    let prevColumnRigthBordersPerRows = createZeroMap();

    units.forEach((ufs, index) => {

      let leftArrowLabelWidthsPerRows = createZeroMap();
      let rightArrowLabelWidthPerRows = createZeroMap();
      let unitMaxTime = -1;

      ufs[1].forEach((f) => {
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

      console.log(prevColumnRigthBordersPerRows)
      console.log(leftArrowLabelWidthsPerRows)
      console.log(rightArrowLabelWidthPerRows)

      for (let i: number = timelineConfig.minTime; i <= timelineConfig.maxTime; i++) {
        const l = prevColumnRigthBordersPerRows.get(i)! + leftArrowLabelWidthsPerRows.get(i)!;
        if (l > unitLeftPosition) {
          unitLeftPosition = l;
        }
      }

      const unitColumnElem = containerElem.querySelector(`[data-unit=${ufs[0]}]`) as HTMLElement;
      const unitWidth = unitColumnElem.getBoundingClientRect().width;
      unitColumnElem.style.left = `${unitLeftPosition}px`;
      unitColumnElem.style.height = `${(unitMaxTime - timelineConfig.minTime + 2) * ROW_HEIGHT}px`;

      for (let i: number = timelineConfig.minTime; i < timelineConfig.maxTime; i++) {
        prevColumnRigthBordersPerRows.set(i, unitLeftPosition + unitWidth + rightArrowLabelWidthPerRows.get(i)!);
      }

      console.log(`GOT NEXT LEFT POS FOR ${ufs[0]}: ${unitLeftPosition}`)
      console.log("UPDATED PREV COLUMN RIGHT BORDERS");
      console.log(prevColumnRigthBordersPerRows);
    })

    // // Adjust spacing between columns based on arrow label widths between neighboring columns in each row
    // const containerElem = containerRef.current;
    // if (!containerElem) return;
    //
    // // Get all arrow label groups from the DOM
    // const arrowLabels = containerElem.querySelectorAll('.arrow-label-group');
    // if (arrowLabels.length === 0) return;
    //
    // const spacingByRow = new Map<number, number>();
    // const containerRect = containerElem.getBoundingClientRect();
    //
    // // Measure arrow labels and group by row
    // arrowLabels.forEach((labelGroup) => {
    //   const labelRect = labelGroup.getBoundingClientRect();
    //   const labelWidth = labelRect.width;
    //
    //   // Calculate which row this label is in (based on Y position)
    //   const relativeY = labelRect.top - containerRect.top + containerElem.scrollTop;
    //   const rowIndex = Math.floor(relativeY / rowHeight);
    //
    //   // Get the text content to determine which connection this is
    //   const textElem = labelGroup.querySelector('text');
    //   if (!textElem) return;
    //
    //   const labelText = textElem.textContent || '';
    //
    //   // Find the corresponding data flow connection to check if it's between neighbors
    //   const connection = dataFlowConnections.find(c => c.variableName === labelText);
    //   if (!connection) return;
    //
    //   // Get source and target functions
    //   const sourceFunc = functions.find(f =>
    //     f.instructions.some(i => i.pID === connection.sourceId)
    //   );
    //   const targetFunc = functions.find(f =>
    //     f.instructions.some(i => i.pID === connection.targetId)
    //   );
    //
    //   if (!sourceFunc || !targetFunc) return;
    //
    //   // Check if they are in neighboring columns
    //   const sourceCol = sourceFunc.column ?? 0;
    //   const targetCol = targetFunc.column ?? 0;
    //
    //   if (Math.abs(sourceCol - targetCol) === 1) {
    //     // This is a neighbor connection, store its label width for this row
    //     const currentMax = spacingByRow.get(rowIndex) || 0;
    //     spacingByRow.set(rowIndex, Math.max(currentMax, labelWidth));
    //   }
    // });
    //
    // // Apply the maximum spacing found across all rows
    // if (spacingByRow.size > 0) {
    //   const maxSpacing = Math.max(...Array.from(spacingByRow.values()));
    //   const requiredGap = maxSpacing + 20; // Add padding
    //
    //   const unitsContainer = containerElem.querySelector('.units-container') as HTMLElement;
    //   if (unitsContainer) {
    //     unitsContainer.style.columnGap = `${requiredGap}px`;
    //   }
    //
    //   // Also set gap on unit-columns
    //   const unitColumns = containerElem.querySelectorAll('.unit-column');
    //   unitColumns?.forEach((col) => {
    //     (col as HTMLElement).style.marginRight = `${requiredGap}px`;
    //   });

      // Update instruction positions after spacing adjustment
      calculateInstructionPositions();
  }, [functions, dataFlowConnections, rowHeight, calculateInstructionPositions]);

  return (
    <div ref={containerRef} className="timeline-container unit-timeline">
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
            const rightEdge = f.width;
            maxWidth = Math.max(maxWidth, rightEdge);
          });

          return (
            // <div key={unitName} className="unit-row">
              <div key={unitName} className="unit-column" data-unit={unitName}>
                <div className="unit-header">
                  <LegendItem
                    componentName={unitName}
                    color={unitColor}
                    enabled={enabledUnits.has(unitName)}
                    onToggle={onUnitToggle}
                  />
                </div>
                <div className="unit-timeline-track" style={maxWidth > 0 ? { width: `${maxWidth}px` } : {}}>
                   {unitFunctions.map(f => (
                     <FunctionRectangle
                       key={f.pID}
                       func={f}
                       bgColor={unitColor}
                       rowHeight={rowHeight}
                       topPadding={topPadding}
                       timelineConfig={timelineConfig}
                       headerMode="inside"
                     />
                   ))}
                </div>
              {/*</div>*/}
            </div>
          );
        })}
      </div>
    </div>
  );
};
