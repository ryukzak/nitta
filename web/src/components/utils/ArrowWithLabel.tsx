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
  const {
    sourceX,
    sourceY,
    sourceHeight,
    targetX,
    targetY,
    targetHeight,
    label,
    color,
  } = props;
  const { pathData } = calculateArrowGeometry(
    sourceX,
    sourceY,
    sourceHeight,
    targetX,
    targetY,
    targetHeight,
    label,
  );

  return (
    <path
      d={pathData}
      stroke={color}
      strokeWidth="2"
      fill="none"
      markerEnd={`url(#arrowhead-${color.replace("#", "")})`}
      className="arrow-path"
    />
  );
};

export const ArrowLabel: FC<ArrowProps> = (props) => {
  const {
    sourceX,
    sourceY,
    sourceHeight,
    targetX,
    targetY,
    targetHeight,
    label,
    color,
  } = props;
  const { textX, textY, rectX, rectY, rectWidth, rectHeight } =
    calculateArrowGeometry(
      sourceX,
      sourceY,
      sourceHeight,
      targetX,
      targetY,
      targetHeight,
      label,
    );

  return (
    <g className="arrow-label-group">
      <rect x={rectX} y={rectY} width={rectWidth} height={rectHeight} />
      <text x={textX} y={textY} className="arrow-label" fill={color}>
        {label}
      </text>
    </g>
  );
};
