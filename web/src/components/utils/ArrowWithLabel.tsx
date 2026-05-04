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
  isNonNeighbor?: boolean;
}

export interface InstructionPosition {
  instructionId: number;
  x: number;
  y: number;
  width: number;
  height: number;
  color: string;
  column?: number;
}

const TEXT_PADDING = 2;
const TEXT_HEIGHT = 14;

const SPACE_BETWEEN_INSTRUCTION_AND_SOURCE_TEXT = 6;
const SPACE_BETWEEN_INSTRUCTION_AND_TARGET_TEXT = 16;

export const getArrowProps = (
  source: InstructionPosition,
  target: InstructionPosition,
  topPadding: number,
  label: string,
) => {
  let arrowSourceX: number;
  let arrowTargetX: number;

  const estimatedTextWidth = label.length * 7;
  const extra_spacing =
    SPACE_BETWEEN_INSTRUCTION_AND_TARGET_TEXT +
    SPACE_BETWEEN_INSTRUCTION_AND_SOURCE_TEXT;

  if (source === undefined) {
    source = {
      instructionId: -1,
      x: target.x - estimatedTextWidth - extra_spacing,
      y: target.y,
      width: 0,
      height: target.height,
      color: "null",
    };
  }

  if (target === undefined) {
    target = {
      instructionId: -1,
      x: source.x + source.width + estimatedTextWidth + extra_spacing,
      y: source.y,
      width: 0,
      height: source.height,
      color: "null",
    };
  }

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

  // Determine if source and target are in non-neighbor columns
  // Non-neighbor means they are not in adjacent columns
  const sourceColumn = source.column ?? 0;
  const targetColumn = target.column ?? 0;
  const isNonNeighbor = Math.abs(sourceColumn - targetColumn) > 1;

  const result: ArrowProps = {
    sourceX: arrowSourceX,
    sourceY: topPadding + source.y,
    sourceHeight: source.height,
    targetX: arrowTargetX,
    targetY: topPadding + target.y,
    targetHeight: target.height,
    label: label,
    color: "#404040",
    isNonNeighbor,
  };
  return result;
};

interface ArrowGeometry {
  pathData: string;
  textX: number;
  textY: number;
  rectX: number;
  rectY: number;
  rectWidth: number;
  rectHeight: number;
  targetTextX?: number;
  targetTextY?: number;
  targetRectX?: number;
  targetRectY?: number;
}

const calculateArrowGeometry = (
  sourceX: number,
  sourceY: number,
  sourceHeight: number,
  targetX: number,
  targetY: number,
  targetHeight: number,
  label: string,
  isNonNeighbor: boolean = false,
): ArrowGeometry => {
  const sourceMidY = sourceY + sourceHeight / 2;
  const targetMidY = targetY + targetHeight / 2;
  const horizontalDistance = targetX - sourceX;
  const estimatedTextWidth = label.length * 6.5;
  let curveOffset = Math.max(30, Math.abs(horizontalDistance) * 0.25);

  let pathData: string;

  curveOffset += isNonNeighbor ? estimatedTextWidth * 1.75 : 45;

  if (sourceX < targetX)
    pathData = `M ${sourceX} ${sourceMidY} C ${sourceX + curveOffset} ${sourceMidY}, ${targetX - curveOffset} ${targetMidY}, ${targetX} ${targetMidY}`;
  else
    pathData = `M ${sourceX} ${sourceMidY} C ${sourceX - curveOffset} ${sourceMidY}, ${targetX + curveOffset} ${targetMidY}, ${targetX} ${targetMidY}`;

  // For neighbor columns, place label in the center of the connection
  // For non-neighbor columns, place label at source side (target side will be added below)
  const centerX = (sourceX + targetX) / 2;
  const centerY = (sourceMidY + targetMidY) / 2;

  // For neighbor columns, use center; for non-neighbor, use source side
  const textX = isNonNeighbor
    ? sourceX < targetX
      ? sourceX + SPACE_BETWEEN_INSTRUCTION_AND_SOURCE_TEXT
      : sourceX - estimatedTextWidth - SPACE_BETWEEN_INSTRUCTION_AND_SOURCE_TEXT
    : centerX - estimatedTextWidth / 2;
  const textY = isNonNeighbor ? sourceMidY + 3 : centerY + 3;

  const rectX = textX - TEXT_PADDING;
  const rectY = textY - TEXT_PADDING - TEXT_HEIGHT * 0.75;
  const rectWidth = estimatedTextWidth;
  const rectHeight = TEXT_HEIGHT + TEXT_PADDING * 2;

  const geometry: ArrowGeometry = {
    pathData,
    textX,
    textY,
    rectX,
    rectY,
    rectWidth,
    rectHeight,
  };

  // For non-neighbor columns, also add label at target side
  if (isNonNeighbor) {
    const targetTextX =
      sourceX < targetX
        ? targetX -
          estimatedTextWidth -
          SPACE_BETWEEN_INSTRUCTION_AND_TARGET_TEXT
        : targetX + SPACE_BETWEEN_INSTRUCTION_AND_TARGET_TEXT;
    const targetTextY = targetMidY + 3;
    const targetRectX = targetTextX - TEXT_PADDING;
    const targetRectY = targetTextY - TEXT_PADDING - TEXT_HEIGHT * 0.75;

    geometry.targetTextX = targetTextX;
    geometry.targetTextY = targetTextY;
    geometry.targetRectX = targetRectX;
    geometry.targetRectY = targetRectY;
  }

  return geometry;
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
    props.isNonNeighbor,
  );

  return (
    <path
      d={pathData}
      stroke={props.color}
      strokeWidth="2"
      fill="none"
      // markerEnd={`url(#arrowhead-${props.color.replace("#", "")})`}
      markerEnd={`url(#arrowhead-${props.label})`}
      className="arrow-path"
    />
  );
};

export const ArrowLabel: FC<ArrowProps> = (props) => {
  const {
    textX,
    textY,
    rectX,
    rectY,
    rectWidth,
    rectHeight,
    targetTextX,
    targetTextY,
    targetRectX,
    targetRectY,
  } = calculateArrowGeometry(
    props.sourceX,
    props.sourceY,
    props.sourceHeight,
    props.targetX,
    props.targetY,
    props.targetHeight,
    props.label,
    props.isNonNeighbor,
  );

  if (
    props.isNonNeighbor &&
    targetTextX !== undefined &&
    targetTextY !== undefined &&
    targetRectX !== undefined &&
    targetRectY !== undefined
  ) {
    // Render labels on both sides for non-neighbor columns
    return (
      <g className="arrow-label-group">
        {/* Source side label */}
        <rect x={rectX} y={rectY} width={rectWidth} height={rectHeight} />
        <text x={textX} y={textY} className="arrow-label" fill={props.color}>
          {props.label}
        </text>
        {/* Target side label */}
        <rect
          x={targetRectX}
          y={targetRectY}
          width={rectWidth}
          height={rectHeight}
        />
        <text
          x={targetTextX}
          y={targetTextY}
          className="arrow-label"
          fill={props.color}
        >
          {props.label}
        </text>
      </g>
    );
  }

  // Single label in center for neighbor columns
  return (
    <g className="arrow-label-group">
      <rect x={rectX} y={rectY} width={rectWidth} height={rectHeight} />
      <text x={textX} y={textY} className="arrow-label" fill={props.color}>
        {props.label}
      </text>
    </g>
  );
};
