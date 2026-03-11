import React, { FC } from 'react';
import type { Instruction, ProcessFunction } from '../utils/ProcessTimeline2';
import { Color, fadeColor } from '../../utils/color';

const ROW_HEIGHT = 70;

interface InstructionRectangleProps {
  instruction: Instruction;
  functionStartTime: number;
  rowHeight: number;
  borderColor: string;
}

export const InstructionRectangle: FC<InstructionRectangleProps> = ({
  instruction,
  functionStartTime,
  rowHeight,
  borderColor,
}) => {
  return (
    <div
      key={instruction.pID}
      data-instruction-id={`instr-${instruction.pID}`}
      className="instruction-rectangle"
      style={{
        top: (instruction.startTime - functionStartTime) * rowHeight + 8,
        height: Math.max(
          50,
          (instruction.endTime - instruction.startTime) * rowHeight - 16,
        ),
        border: `2px solid ${borderColor}`,
      }}
      title={instruction.info}
    >
      <div className="instruction-content">
        <div className="instruction-label">
          <strong>
            {instruction.label} #{instruction.pID}
          </strong>
        </div>
        <div className="instruction-time">
          [{instruction.startTime};{instruction.endTime}]
        </div>
        <div className="instruction-io-info">
          ({Array.from(instruction.inputs).join(",")}) -{">"} (
          {Array.from(instruction.outputs).join(",")})
        </div>
      </div>
    </div>
  );
};

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
}

interface FunctionBlockContentProps {
  func: ProcessFunction;
  bgColor: Color;
  headerHeight: number;
  rowHeight?: number;
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
}) => {
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

  return (
    <div
      key={func.pID}
      className="function-rectangle"
      style={{
        top:
          topPadding +
          (func.startTime - timelineConfig.minTime) * rowHeight -
          headerHeight,
        height:
          (func.endTime - func.startTime + 1) * rowHeight +
          headerHeight,
        left: `${leftPosition}px`,
        width: `${func.width}px`,
        borderColor: bgColor.toHexString(),
        backgroundColor: bgTransparentColor.toHexString(),
      }}
    >
      <FunctionBlockContent
        func={func}
        bgColor={bgColor}
        headerHeight={headerHeight}
        rowHeight={rowHeight}
      />
    </div>
  );
};

export const FunctionBlockContent: FC<FunctionBlockContentProps> = ({
  func,
  bgColor,
  headerHeight,
  rowHeight = ROW_HEIGHT,
  showHeader = true,
  showInstructions = true,
}) => {
  return (
    <>
      {showHeader && (
        <div
          data-header-id={`func-header-${func.pID}`}
          className="function-header"
          style={{
            backgroundColor: fadeColor(bgColor, 0x15 / 255).toHexString(),
            color: bgColor.toHexString(),
            height: "auto",
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

      {showInstructions && (
        <div className="instructions-container">
          {func.instructions.map((instr) => (
            <InstructionRectangle
              key={instr.pID}
              instruction={instr}
              functionStartTime={func.startTime}
              rowHeight={rowHeight}
              borderColor={bgColor.toHexString()}
            />
          ))}
        </div>
      )}
    </>
  );
};
