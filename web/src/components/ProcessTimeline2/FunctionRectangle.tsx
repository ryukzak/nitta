import React, { FC } from 'react';
import {Instruction, ProcessFunction, TEXT_PADDING} from '../utils/ProcessTimeline2';
import { Color, fadeColor } from '../../utils/color';
import './FunctionRectangle.scss';

const ROW_HEIGHT = 70;

interface FunctionRectangleProps {
  func: ProcessFunction;
  bgColor: Color;
  headerHeight: number;
  rowHeight: number;
  topPadding: number;
  timelineConfig: {
    minTime: number;
    maxTime: number;
  };
  mostLeftFreeSpacesInColumnsPerRows: Map<number, Map<number, number>>;
  headerMode?: 'inside' | 'outside';
  showHeader?: boolean;
  showInstructions?: boolean;
}

export const FunctionRectangle: FC<FunctionRectangleProps> = ({
  func,
  bgColor,
  headerHeight,
  rowHeight,
  topPadding,
  timelineConfig,
  mostLeftFreeSpacesInColumnsPerRows,
  headerMode = 'outside',
  showHeader = true,
  showInstructions = true,
}) => {

  const instructionContainerRef = React.useRef<HTMLDivElement>(null)
  const [instructionContainerHeight, setInstructionContainerHeight] = React.useState<number | null>(null)

  const bgTransparentColor = new Color({
    r: bgColor.obj.r,
    g: bgColor.obj.g,
    b: bgColor.obj.b,
  });
  bgTransparentColor.obj.a = 0x15 / 255;

  let leftPosition = 0;

  for (
    let i = Math.max(
      func.startTime - Math.ceil(headerHeight / rowHeight),
      -1,
    );
    i <= func.endTime;
    i++
  ) {
    const rowSpaces = mostLeftFreeSpacesInColumnsPerRows.get(i);
    if (rowSpaces) {
      const prevColSpace = rowSpaces.get(func.column - 1);
      if (prevColSpace !== undefined) {
        leftPosition = Math.max(leftPosition, prevColSpace);
      }
    }
  }

  React.useEffect(() => {
    if (instructionContainerRef.current) {
      const resizeObserver = new ResizeObserver((entries) => {
        for (const entry of entries) {
          setInstructionContainerHeight(entry.contentRect.height)
        }
      })
      resizeObserver.observe(instructionContainerRef.current)
      return () => resizeObserver.disconnect()
    }
  }, [])

  return (
    <div
      key={func.pID}
      className={`function-rectangle`}
      style={{
        top:
          topPadding +
          (func.startTime - timelineConfig.minTime) * rowHeight -
          ((headerMode === "outside") ? headerHeight : 0),
        height:
          (func.endTime - func.startTime + 1) * rowHeight +
          ((headerMode === "outside") ? headerHeight : 0),
        left: `${leftPosition}px`,
        // width: `${func.width}px`,
        borderColor: bgColor.toHexString(),
        backgroundColor: bgTransparentColor.toHexString(),
      }}
    >
      {/* Top Header - shown when headerMode is 'top' */}
      {showHeader && (
        <div
          data-header-id={`func-header-${func.pID}`}
          className="function-header"
          style={{
            backgroundColor: fadeColor(bgColor, 0x15 / 255).toHexString(),
            color: bgColor.toHexString(),
            height: 'auto',
            width: func.instructionMaxWidth + TEXT_PADDING,
            minHeight: `${headerHeight}px`,
          }}
        >
          <div className="function-name">
            <div>
              {func.component} #{func.pID}
            </div>
            <div>{func.label}</div>
          </div>
          <div className="function-time">
            [{func.startTime};{func.endTime}]
          </div>
        </div>
      )}

      {/* Instructions Container */}
      {showInstructions && (
        <div ref={instructionContainerRef} className="instructions-container" style={{width: func.instructionMaxWidth + TEXT_PADDING}}>
          {func.instructions.map((instruction) => (
            <div
              key={instruction.pID}
              data-instruction-id={`instr-${instruction.pID}`}
              className="instruction-rectangle"
              style={{
                top: (instruction.startTime - func.startTime) * rowHeight + 8,
                height: Math.max(
                  50,
                  (instruction.endTime - instruction.startTime) * rowHeight - 16,
                ),
                border: `2px solid ${bgColor.toHexString()}`,
              }}
              title={instruction.info}
            >
              <div className="instruction-content">
                <div className="instruction-label">
                  <strong>
                    {instruction.label}{/* #{instruction.pID}*/}
                  </strong>
                </div>
                {/*<div className="instruction-time">*/}
                {/*  [{instruction.startTime};{instruction.endTime}]*/}
                {/*</div>*/}
                <div className="instruction-io-info">
                  ({Array.from(instruction.inputs).join(',')}) -&gt; (
                  {Array.from(instruction.outputs).join(',')})
                </div>
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  );
};
