import React, {
  type FC,
  useCallback,
  useLayoutEffect,
  useState,
} from "react";
import {
  COLUMN_MARGIN, CONTAINER_BUTTOM_PADDING,
  DataFlowConnection, MIN_FUNCTION_GAP,
  ProcessFunction, ROW_HEIGHT,
  LabelPosition, instructionPositionsEqual,
  estimateArrowTextWidth,
  mapsEqual,
  getInstructionColumnByPID,
  getArrowLabelPosition,
  assignInputOutputPositions,
  calculateInstructionPositionsFromDOM,
} from "../utils/ProcessTimeline2";
import type { InstructionPosition } from "../utils/ArrowWithLabel";
import type { Color } from "../../utils/color";
import { FunctionRectangle } from "./FunctionRectangle";
import { DataFlowOverlay } from "./DataFlows";
import "components/ProcessTimeline2/TimelineContainer.scss";



interface FunctionTimelineProps {
  functions: ProcessFunction[];
  timelineConfig: { minTime: number; maxTime: number };
  dataFlowConnections: DataFlowConnection[];
  getComponentColor: (component: string) => Color;
  onLayoutComplete: (
    containerHeight: number,
    topPadding: number
  ) => void;
  selectedInstructionId: number | null;
  selectedDataFlowId: string | null;
  onInstructionSelect: (instructionId: number) => void;
  onDataFlowSelect: (dataFlowId: string) => void;
  getRelatedDataFlows: (instructionId: number) => string[];
  getRelatedInstructions: (dataFlowId: string) => number[];
  onClearSelection?: () => void;
}

export const FunctionTimeline: FC<FunctionTimelineProps> = ({
  functions,
  timelineConfig,
  dataFlowConnections,
  getComponentColor,
  onLayoutComplete,
  selectedInstructionId,
  selectedDataFlowId,
  onInstructionSelect,
  onDataFlowSelect,
  getRelatedDataFlows,
  getRelatedInstructions,
  onClearSelection,
}) => {
  const [layoutFunctions, setLayoutFunctions] = useState<ProcessFunction[]>([]);
  const [containerHeight, setContainerHeight] = useState(1000);
  const [instructionPositions, setInstructionPositions] = useState<
    Map<number, InstructionPosition>
  >(new Map());
  const [headerHeights, setHeaderHeights] = useState<Map<number, number>>(
    new Map(),
  );
  const containerRef = React.useRef<HTMLDivElement>(null);
  const [topPadding, setTopPadding] = useState(0);
  const [functionColumns, setFunctionColumns] = useState<Map<number, number>>(new Map());
  const processingRef = React.useRef(false);

  const calculateHeaderHeights = useCallback(() => {
    const container = containerRef.current;
    if (!container) return;

    const heightsMap = new Map<number, number>();

    functions.forEach((func) => {
      const headerId = `func-header-${func.pID}`;
      const headerElement = container.querySelector(
        `[data-header-id="${headerId}"]`,
      ) as HTMLElement;

      if (headerElement) {
        const measuredHeight = headerElement.offsetHeight;
        heightsMap.set(func.pID, measuredHeight);
      } else {
        heightsMap.set(func.pID, ROW_HEIGHT);
      }
    });

    setHeaderHeights((prevHeights) => {
      if (mapsEqual(prevHeights, heightsMap)) {
        return prevHeights;
      }
      return heightsMap;
    });
  }, [functions, mapsEqual]);

  const calculateInstructionPositions = useCallback(() => {
    const container = containerRef.current;
    const positionsMap = calculateInstructionPositionsFromDOM(
      container,
      functions,
      getComponentColor,
      (func) => functionColumns.get(func.pID) ?? 0,
    );

    setInstructionPositions((prevPositions) => {
      if (instructionPositionsEqual(prevPositions, positionsMap)) {
        return prevPositions;
      }
      return positionsMap;
    });
  }, [functions, functionColumns, getComponentColor]);

  const getFunctionAffectedRows = (func: ProcessFunction) => {
    const headerHeight = headerHeights.get(func.pID) || ROW_HEIGHT;
    const headerRowHeight = Math.ceil(headerHeight / ROW_HEIGHT);

    const firstAffectedRowIndex = Math.max(
      func.startTime - headerRowHeight,
      -1,
    );
    const lastAffectedRowIndex = func.endTime;
    return { firstAffectedRowIndex: firstAffectedRowIndex, lastAffectedRowIndex: lastAffectedRowIndex }
  }

  const handleContainerClick = (e: React.MouseEvent<HTMLDivElement>) => {
    const target = e.target as HTMLElement;

    // Check if we clicked on an interactive element (instruction, data flow arrow, etc)
    let isOnInteractive = false;
    let currentElement: Element | null = target;

    while (currentElement && currentElement !== containerRef.current) {
      if (currentElement.classList.contains('instruction-rectangle') ||
          currentElement.classList.contains('function-rectangle') ||
          currentElement.classList.contains('dataflow-group') ||
          currentElement.tagName === 'polyline' ||
          currentElement.tagName === 'path') {
        isOnInteractive = true;
        break;
      }
      currentElement = currentElement.parentElement;
    }

    // If not on an interactive element, clear selection
    if (!isOnInteractive) {
      onClearSelection?.();
    }
  };

  const functionsArray = functions.map((f) => ({ ...f }));

  const performLayout = useCallback(() => {
    if (functions.length === 0 || headerHeights.size < functions.length) return;
    if (processingRef.current) return;
    processingRef.current = true;

    functionsArray.sort((a, b) => a.startTime - b.startTime);

    type Interval = [number, number];
    const occupiedIntervalsPerColumn = new Map<number, Interval[]>();

    // place functions into columns
    functionsArray.forEach((func) => {
      let assignedColumn = 0;
      const headerHeight = headerHeights.get(func.pID) || ROW_HEIGHT;
      const headerRowHeight = Math.ceil(headerHeight / ROW_HEIGHT);

      while (true) {
        if (!occupiedIntervalsPerColumn.has(assignedColumn)) {
          const intervalArray: Interval[] = [];
          intervalArray.push([
            Math.max(func.startTime - headerRowHeight, -1),
            func.endTime + MIN_FUNCTION_GAP,
          ]);
          occupiedIntervalsPerColumn.set(assignedColumn, intervalArray);
          func.column = assignedColumn;
          break;
        }
        let isOverlapping = false;
        const intervals = occupiedIntervalsPerColumn.get(assignedColumn);
        if (intervals) {
          for (const interval of intervals) {
            const newFuncStart = func.startTime - headerRowHeight;
            const newFuncEnd = func.endTime + MIN_FUNCTION_GAP;
            if (!(newFuncEnd < interval[0] || newFuncStart > interval[1])) {
              isOverlapping = true;
              break;
            }
          }
        }
        if (isOverlapping) {
          assignedColumn += 1;
        } else {
          func.column = assignedColumn;
          const newInterval: Interval = [
            func.startTime - headerRowHeight,
            func.endTime + MIN_FUNCTION_GAP,
          ];
          occupiedIntervalsPerColumn.get(assignedColumn)?.push(newInterval);
          break;
        }
      }
    });

    functionsArray.sort(
      (a, b) => a.column - b.column || a.startTime - b.startTime,
    );

    // assign input/output positions
    assignInputOutputPositions(functionsArray);

    const { minTime, maxTime } = timelineConfig;

    // align function blocks to the most left position to minimize width
    const funcLeftPositions = new Map<number, number>();

    functionsArray.forEach((func) => {
      const {firstAffectedRowIndex, lastAffectedRowIndex} = getFunctionAffectedRows(func);

      let leftOccupiedPositionsPerRow = new Map<number, number>();
      for (let i: number = func.startTime; i <= func.endTime; i++)
        leftOccupiedPositionsPerRow.set(i, 0)

      functionsArray.forEach((f) => {
        if (f.column === func.column - 1) {
          const {firstAffectedRowIndex: a, lastAffectedRowIndex: b} = getFunctionAffectedRows(f);

          if (Math.max(firstAffectedRowIndex, a) <= Math.min(lastAffectedRowIndex, b)) {

            let neighbourRightBorderPosition = funcLeftPositions.get(f.pID)! + f.width;
            for (let i: number = f.startTime; i <= f.endTime; i++)
              leftOccupiedPositionsPerRow.set(i, neighbourRightBorderPosition + COLUMN_MARGIN)

            f.instructions.forEach((i) => {
              i.outputs.forEach((l) => {
                if (i.outputPositions.get(l) === LabelPosition.Right) {
                  const labelRightBorder = neighbourRightBorderPosition + estimateArrowTextWidth(l);
                  leftOccupiedPositionsPerRow.set(i.startTime, labelRightBorder)
                }
              })
              i.inputs.forEach((l) => {
                if (i.inputPositions.get(l) === LabelPosition.Right) {
                  const labelRightBorder = neighbourRightBorderPosition + estimateArrowTextWidth(l);
                  leftOccupiedPositionsPerRow.set(i.startTime, labelRightBorder)
                }
              })
            })
          }
        }
      })
      func.instructions.forEach((i) => {
        i.inputs.forEach((l) => {
          if (i.inputPositions.get(l) === LabelPosition.Left) {
            const lastPos = leftOccupiedPositionsPerRow.get(i.startTime);
            const nextPos = lastPos! + estimateArrowTextWidth(l);
            leftOccupiedPositionsPerRow.set(i.startTime, nextPos);
          }
        })
        i.outputs.forEach((l) => {
          if (i.outputPositions.get(l) === LabelPosition.Left) {
            const lastPos = leftOccupiedPositionsPerRow.get(i.startTime);
            const nextPos = lastPos! + estimateArrowTextWidth(l);
            leftOccupiedPositionsPerRow.set(i.startTime, nextPos);
          }
        })
      })
      func.leftPosition = Math.max(...leftOccupiedPositionsPerRow.values());
      funcLeftPositions.set(func.pID, func.leftPosition);
    });

    const functionsAtMinTime = functionsArray.filter(
      (f) => f.startTime === minTime,
    );
    const maxHeaderHeightAtMin = Math.max(
      ...functionsAtMinTime.map((f) => headerHeights.get(f.pID) || ROW_HEIGHT),
      0,
    );

    const newContainerHeight =
      maxHeaderHeightAtMin +
      (maxTime - minTime + 1) * ROW_HEIGHT +
      CONTAINER_BUTTOM_PADDING;
    const newTopPadding = Math.max(maxHeaderHeightAtMin, ROW_HEIGHT);

    setContainerHeight(newContainerHeight);
    setTopPadding(newTopPadding);

    // Store column assignments
    const columnsMap = new Map<number, number>();
    functionsArray.forEach((func) => {
      columnsMap.set(func.pID, func.column);
    });
    setFunctionColumns(columnsMap);

    // Notify parent component of layout changes
    onLayoutComplete(
      newContainerHeight,
      newTopPadding
    );

    processingRef.current = false;

    setLayoutFunctions(functionsArray);
  }, [headerHeights, timelineConfig]);

  useLayoutEffect(() => {
    performLayout();
  }, [performLayout]);

  useLayoutEffect(() => {
    calculateHeaderHeights();
    calculateInstructionPositions();
  }, [calculateHeaderHeights, calculateInstructionPositions]);

  return (
    <div
      className="timeline-container function-timeline"
      ref={containerRef}
      onClick={handleContainerClick}
      style={{
        minHeight: `${containerHeight}px`,
        // paddingTop: `${topPadding}px`,
      }}
    >
      {/* SVG overlay for data flow arrows and grid lines */}
      <DataFlowOverlay
        dataFlowConnections={dataFlowConnections}
        instructionPositions={instructionPositions}
        topPadding={topPadding}
        timelineConfig={timelineConfig}
        selectedInstructionId={selectedInstructionId}
        selectedDataFlowId={selectedDataFlowId}
        onDataFlowSelect={onDataFlowSelect}
        getRelatedInstructions={getRelatedInstructions}
      />

      {layoutFunctions.map((func) => {
        const bgColor = getComponentColor(func.component);
        return (
          <FunctionRectangle
            key={func.pID}
            func={func}
            bgColor={bgColor}
            topPadding={topPadding}
            timelineConfig={timelineConfig}
            selectedInstructionId={selectedInstructionId}
            selectedDataFlowId={selectedDataFlowId}
            onInstructionSelect={onInstructionSelect}
            getRelatedDataFlows={getRelatedDataFlows}
            getRelatedInstructions={getRelatedInstructions}
          />
        );
      })}
    </div>
  );
};
