import React, {
  type FC,
  useCallback,
  useEffect,
  useLayoutEffect,
  useState,
} from "react";
import type {
  DataFlowConnection,
  Instruction,
  ProcessFunction,
} from "../utils/ProcessTimeline2";
import { OutputPosition } from "../utils/ProcessTimeline2";
import type { InstructionPosition } from "../utils/ArrowWithLabel";
import type { Color } from "../../utils/color";
import type { ProcessData } from "services/HaskellApiService";
import type { ProcessTimelines } from "services/gen/types";
import { FunctionRectangle } from "./FunctionRectangle";
import { DataFlowOverlay } from "./DataFlows";
import { estimateArrowTextWidth } from "../utils/ProcessTimeline2";
import "components/ProcessTimeline2/FunctionTimeline.scss";

const ROW_HEIGHT = 70;
const COLUMN_MARGIN = 20;
const MIN_FUNCTION_GAP = 0.5;
const CONTAINER_BUTTOM_PADDING = ROW_HEIGHT;

interface FunctionTimelineProps {
  functions: ProcessFunction[];
  timelineConfig: { minTime: number; maxTime: number };
  dataFlowConnections: DataFlowConnection[];
  getComponentColor: (component: string) => Color;
  onLayoutComplete: (
    functions: ProcessFunction[],
    containerHeight: number,
    topPadding: number,
    mostLeftFreeSpaces: Map<number, Map<number, number>>,
    instructionPositions: Map<number, InstructionPosition>,
  ) => void;
}

export const FunctionTimeline: FC<FunctionTimelineProps> = ({
  functions,
  timelineConfig,
  dataFlowConnections,
  getComponentColor,
  onLayoutComplete,
}) => {
  const [containerHeight, setContainerHeight] = useState(1000);
  const [instructionPositions, setInstructionPositions] = useState<
    Map<number, InstructionPosition>
  >(new Map());
  const [headerHeights, setHeaderHeights] = useState<Map<number, number>>(
    new Map(),
  );
  const containerRef = React.useRef<HTMLDivElement>(null);
  const [
    mostLeftFreeSpacesInColumnsPerRows,
    setMostLeftFreeSpacesInColumnsPerRows,
  ] = useState<Map<number, Map<number, number>>>(new Map());
  const [topPadding, setTopPadding] = useState(0);
  const processingRef = React.useRef(false);

  const mapsEqual = useCallback(
    (map1: Map<number, number>, map2: Map<number, number>): boolean => {
      if (map1.size !== map2.size) return false;
      for (const [key, value] of map1) {
        if (map2.get(key) !== value) return false;
      }
      return true;
    },
    [],
  );

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
        heightsMap.set(func.pID, Math.max(measuredHeight, 50));
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

  const instructionPositionsEqual = useCallback(
    (
      map1: Map<number, InstructionPosition>,
      map2: Map<number, InstructionPosition>,
    ): boolean => {
      if (map1.size !== map2.size) return false;
      for (const [key, pos1] of map1) {
        const pos2 = map2.get(key);
        if (
          !pos2 ||
          pos1.x !== pos2.x ||
          pos1.y !== pos2.y ||
          pos1.width !== pos2.width ||
          pos1.height !== pos2.height ||
          pos1.color !== pos2.color
        ) {
          return false;
        }
      }
      return true;
    },
    [],
  );

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

  const performLayout = useCallback(() => {
    if (functions.length === 0 || headerHeights.size < functions.length) return;
    if (processingRef.current) return;
    processingRef.current = true;

    const functionsArray = functions.map((f) => ({ ...f }));
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

    const getInstructionColumnByPID = (PID: number) => {
      for (const f of functionsArray) {
        for (const i of f.instructions) {
          if (i.pID === PID) return f.column;
        }
      }
      return null;
    };

    functionsArray.forEach((f) => {
      f.instructions.forEach((i) => {
        for (const [, targetPID] of i.sendsOutputsToPIDs) {
          const targetInstructionColumn = getInstructionColumnByPID(targetPID);
          if (targetInstructionColumn === null) continue;
          let nextOutputPosition: OutputPosition;
          if (targetInstructionColumn >= f.column)
            nextOutputPosition = OutputPosition.Right;
          else nextOutputPosition = OutputPosition.Left;

          if (i.outputPosition === OutputPosition.None) {
            i.outputPosition = nextOutputPosition;
          } else if (i.outputPosition !== nextOutputPosition) {
            i.outputPosition = OutputPosition.Both;
          }
        }
      });
    });

    const { minTime, maxTime } = timelineConfig;

    // align function blocks to the most left position to minimize width
    const mostLeftFreeSpaceInColumnsByRows = new Map<
      number,
      Map<number, number>
    >();
    const maxCol = Math.max(...functionsArray.map((f) => f.column), 0);

    for (let i = minTime - 2; i <= maxTime; i++) {
      const columnsMap = new Map<number, number>();
      columnsMap.set(-1, 0);
      for (let j = 0; j <= maxCol; j++) columnsMap.set(j, COLUMN_MARGIN);
      mostLeftFreeSpaceInColumnsByRows.set(i, columnsMap);
    }

    functionsArray.forEach((func) => {
      const headerHeight = headerHeights.get(func.pID) || ROW_HEIGHT;
      const headerRowHeight = Math.ceil(headerHeight / ROW_HEIGHT);

      const firstAffectedRowIndex = Math.max(
        func.startTime - headerRowHeight,
        -1,
      );
      const lastAffectedRowIndex = func.endTime;

      const instructionPerRow = new Map<number, Instruction>();
      for (const instr of func.instructions) {
        for (let i = instr.startTime; i <= instr.endTime; i++) {
          instructionPerRow.set(i, instr);
        }
      }

      let currentLeftPositionOfFunction = 0;
      if (func.column !== 0) {
        for (let i = firstAffectedRowIndex; i < lastAffectedRowIndex + 1; i++) {
          let leftArrowTextWidth = 0;
          let arrowLabel = "";
          if (instructionPerRow.has(i)) {
            const instr = instructionPerRow.get(i)!;
            arrowLabel = instr.sendsOutputsToPIDs.keys().toArray()[0];
            if (
              instr.outputPosition === OutputPosition.Left ||
              instr.outputPosition === OutputPosition.Both
            )
              leftArrowTextWidth = estimateArrowTextWidth(arrowLabel);
          }
          currentLeftPositionOfFunction = Math.max(
            currentLeftPositionOfFunction,
            mostLeftFreeSpaceInColumnsByRows.get(i)!.get(func.column - 1)! +
              leftArrowTextWidth,
          );
          const prevColumnWithLeftArrow =
            mostLeftFreeSpaceInColumnsByRows.get(i)!.get(func.column - 1)! +
            leftArrowTextWidth;
          mostLeftFreeSpaceInColumnsByRows
            .get(i)!
            .set(func.column - 1, prevColumnWithLeftArrow);
        }
      }
      for (let i = firstAffectedRowIndex; i < lastAffectedRowIndex + 1; i++) {
        let rightArrowTextWidth = 0;
        if (instructionPerRow.has(i)) {
          const instr = instructionPerRow.get(i)!;
          if (
            instr.outputPosition === OutputPosition.Right ||
            instr.outputPosition === OutputPosition.Both
          )
            rightArrowTextWidth = estimateArrowTextWidth(
              instr.sendsOutputsToPIDs.keys().toArray()[0],
            );
        }
        const spaceBetweenColumns =
          rightArrowTextWidth !== 0 ? rightArrowTextWidth : COLUMN_MARGIN;
        const nextColumnRightBorder =
          currentLeftPositionOfFunction + func.width + spaceBetweenColumns;
        mostLeftFreeSpaceInColumnsByRows
          .get(i)!
          .set(func.column, nextColumnRightBorder);
      }
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
    const newTopPadding = maxHeaderHeightAtMin;

    setContainerHeight(newContainerHeight);
    setMostLeftFreeSpacesInColumnsPerRows(mostLeftFreeSpaceInColumnsByRows);
    setTopPadding(newTopPadding);

    // Notify parent component of layout changes
    onLayoutComplete(
      functionsArray,
      newContainerHeight,
      newTopPadding,
      mostLeftFreeSpaceInColumnsByRows,
      instructionPositions,
    );

    processingRef.current = false;
  }, [headerHeights, timelineConfig, onLayoutComplete]);

  useLayoutEffect(() => {
    if (functions.length > 0) {
      calculateHeaderHeights();
    }
  }, [functions, calculateHeaderHeights]);

  useLayoutEffect(() => {
    performLayout();
  }, [performLayout]);

  useLayoutEffect(() => {
    if (containerHeight > 0) calculateInstructionPositions();
  }, [containerHeight, calculateInstructionPositions]);

  return (
    <div
      className="diagram-container"
      ref={containerRef}
      style={{
        minHeight: `${containerHeight}px`,
        paddingTop: `${topPadding}px`,
      }}
    >
      {/* SVG overlay for data flow arrows and grid lines */}
      <DataFlowOverlay
        dataFlowConnections={dataFlowConnections}
        instructionPositions={instructionPositions}
        topPadding={topPadding}
        timelineConfig={timelineConfig}
        rowHeight={ROW_HEIGHT}
      />

      {functions.map((func) => {
        const bgColor = getComponentColor(func.component);
        const headerHeight = headerHeights.get(func.pID) || ROW_HEIGHT;

        return (
          <FunctionRectangle
            key={func.pID}
            func={func}
            bgColor={bgColor}
            headerHeight={headerHeight}
            rowHeight={ROW_HEIGHT}
            topPadding={topPadding}
            timelineConfig={timelineConfig}
            mostLeftFreeSpacesInColumnsPerRows={mostLeftFreeSpacesInColumnsPerRows}
          />
        );
      })}
    </div>
  );
};
