import React, { FC } from 'react';
import {ProcessFunction, ROW_HEIGHT, TEXT_PADDING} from '../utils/ProcessTimeline2';
import { Color, fadeColor } from '../../utils/color';
import './FunctionRectangle.scss';

interface FunctionRectangleProps {
  func: ProcessFunction;
  bgColor: Color;
  topPadding: number;
  timelineConfig: {
    minTime: number;
    maxTime: number;
  };
  headerMode?: 'inside' | 'outside';
  showHeader?: boolean;
  showInstructions?: boolean;
}

export const FunctionRectangle: FC<FunctionRectangleProps> = ({
  func,
  bgColor,
  topPadding,
  timelineConfig,
  headerMode = 'outside',
  showHeader = true,
  showInstructions = true,
}) => {

  const headerRef = React.useRef<HTMLDivElement | null>(null);
  const [headerHeight, setHeaderHeight] = React.useState<number>(0);

  const bgTransparentColor = new Color({
    r: bgColor.obj.r,
    g: bgColor.obj.g,
    b: bgColor.obj.b,
  });
  bgTransparentColor.obj.a = 0x15 / 255;

  React.useLayoutEffect(() => {
    if (headerRef.current) {
      const rect = headerRef.current.getBoundingClientRect();
      setHeaderHeight(rect.height);
    }
  }, []); // run after first render

  return (
    <div
      key={func.pID}
      className={`function-rectangle`}
      style={{
        top:
          topPadding +
          (func.startTime - timelineConfig.minTime) * ROW_HEIGHT -
          ((headerMode === "outside" && showHeader) ? headerHeight : 0),
        height:
          (func.endTime - func.startTime + 1) * ROW_HEIGHT +
          ((headerMode === "outside" && showHeader) ? headerHeight : 0),
        left: `${func.leftPosition}px`, //`${leftPosition}px`,
        // width: `${func.width}px`,
        borderColor: bgColor.toHexString(),
        backgroundColor: bgTransparentColor.toHexString(),
      }}
    >
      {/* Top Header - shown when headerMode is 'top' */}
      {showHeader && (
        <div
          ref={headerRef}
          data-header-id={`func-header-${func.pID}`}
          className={`function-header ${(headerMode === 'inside' ? "inner" : "")}`}
          style={{
            backgroundColor: fadeColor(bgColor, 0x15 / 255).toHexString(),
            color: bgColor.toHexString(),
            // height: 'auto',
            width: func.instructionMaxWidth + TEXT_PADDING,
            height: `${headerMode === 'inside' ? ROW_HEIGHT / 5 + "px" : "auto"}`,
          }}
        >
          <div className="function-name">
            {/*<div>*/}
            {/*  {func.component} #{func.pID}*/}
            {/*</div>*/}
            <div>{func.label}</div>
          </div>
          <div className="function-time">
            {/*[{func.startTime};{func.endTime}]*/}
          </div>
        </div>
      )}

      {/* Instructions Container */}
      {showInstructions && (
        <div className="instructions-container" style={{width: func.instructionMaxWidth + TEXT_PADDING}}>
          {func.instructions.map((instruction, index) => (
            <div
              key={instruction.pID}
              data-instruction-id={`instr-${instruction.pID}`}
              className="instruction-rectangle"
              style={{
                top: (instruction.startTime - func.startTime) * ROW_HEIGHT + 8 - ((headerMode === 'inside' && index !== 0) ? (ROW_HEIGHT / 5) : 0),
                height: Math.max(
                  1,
                  (instruction.endTime - instruction.startTime + 1) * ROW_HEIGHT - 20 - ((headerMode === 'inside' && index === 0) ? (ROW_HEIGHT / 5) : 0),
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
