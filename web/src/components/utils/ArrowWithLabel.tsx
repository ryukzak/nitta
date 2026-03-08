import React, { type FC } from "react";

export interface ArrowProps {
  sourceX: number;
  sourceY: number;
  sourceHeight: number;
  targetX: number;
  targetY: number;
  targetHeight: number;
  label: string;
  color: string;
}

export interface InstructionPosition {
  instructionId: number;
  x: number;
  y: number;
  width: number;
  height: number;
  color: string;
}

export const getArrowProps = (
  source: InstructionPosition,
  target: InstructionPosition,
  topPadding: number,
  label: string,
) => {
  let arrowSourceX: number;
  let arrowTargetX: number;

  if (source.x > target.x) {
    arrowSourceX = source.x;
    arrowTargetX = target.x + target.width;
  } else if (source.x === target.x) {
    arrowSourceX = source.x + source.width;
    arrowTargetX = target.x + target.width;
  } else {
    arrowSourceX = source.x + source.width;
    arrowTargetX = target.x;
  }
  const result: ArrowProps = {
    sourceX: arrowSourceX,
    sourceY: topPadding + source.y,
    sourceHeight: source.height,
    targetX: arrowTargetX,
    targetY: topPadding + target.y,
    targetHeight: target.height,
    label: label,
    color: "#404040",
  };
  return result;
};

const calculateArrowGeometry = (
  sourceX: number,
  sourceY: number,
  sourceHeight: number,
  targetX: number,
  targetY: number,
  targetHeight: number,
  label: string,
) => {
  const sourceMidY = sourceY + sourceHeight / 2;
  const targetMidY = targetY + targetHeight / 2;
  const horizontalDistance = targetX - sourceX;
  const estimatedTextWidth = label.length * 7;
  const curveOffset = Math.max(30, Math.abs(horizontalDistance) * 0.25);

  let pathData: string;
  if (sourceX < targetX)
    pathData = `M ${sourceX} ${sourceMidY} C ${sourceX + estimatedTextWidth * 1.75 + curveOffset} ${sourceMidY}, ${targetX - 45 - curveOffset} ${targetMidY}, ${targetX} ${targetMidY}`;
  else
    pathData = `M ${sourceX} ${sourceMidY} C ${sourceX - estimatedTextWidth * 1.75 - curveOffset} ${sourceMidY}, ${targetX + 45 + curveOffset} ${targetMidY}, ${targetX} ${targetMidY}`;

  const textPaddingX = 2;
  const textPaddingY = 2;
  const textX = sourceX < targetX ? sourceX + 8 : sourceX - estimatedTextWidth;
  const textY = sourceMidY + 3;

  const rectX = textX - textPaddingX;
  const rectY = textY - textPaddingY - 6;
  const rectWidth = estimatedTextWidth;
  const rectHeight = 14 + textPaddingY * 2;

  return { pathData, textX, textY, rectX, rectY, rectWidth, rectHeight };
};

export const ArrowPath: FC<ArrowProps> = (props) => {
  const { pathData } = calculateArrowGeometry(
    props.sourceX,
    props.sourceY,
    props.sourceHeight,
    props.targetX,
    props.targetY,
    props.targetHeight,
    props.label,
  );

  return (
    <path
      d={pathData}
      stroke={props.color}
      strokeWidth="2"
      fill="none"
      markerEnd={`url(#arrowhead-${props.color.replace("#", "")})`}
      className="arrow-path"
    />
  );
};

export const ArrowLabel: FC<ArrowProps> = (props) => {
  const { textX, textY, rectX, rectY, rectWidth, rectHeight } =
    calculateArrowGeometry(
      props.sourceX,
      props.sourceY,
      props.sourceHeight,
      props.targetX,
      props.targetY,
      props.targetHeight,
      props.label,
    );
  return (
    <g className="arrow-label-group">
      <rect x={rectX} y={rectY} width={rectWidth} height={rectHeight} />
      <text x={textX} y={textY} className="arrow-label" fill={props.color}>
        {props.label}
      </text>
    </g>
  );
};
