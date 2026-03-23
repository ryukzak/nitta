import React, { type FC } from "react";
import type {
  DataFlowConnection,
} from "../utils/ProcessTimeline2";
import {
  ArrowLabel,
  ArrowPath,
  getArrowProps,
  type InstructionPosition,
} from "../utils/ArrowWithLabel";
import './DataFlows.scss';

interface DataFlowOverlayProps {
  topPadding: number;
  timelineConfig: {
    minTime: number;
    maxTime: number;
  };
  rowHeight: number;
  dataFlowConnections?: DataFlowConnection[];
  instructionPositions?: Map<number, InstructionPosition>;
}

export const DataFlowOverlay: FC<DataFlowOverlayProps> = ({
  topPadding,
  timelineConfig,
  rowHeight,
  dataFlowConnections = [],
  instructionPositions = new Map(),
}) => {
  return (
    <svg className="data-flow-overlay" role={"presentation"} style={{ width: '100%', height: '100%' }}>
      <defs>
        {Array.from(
          new Set(
            dataFlowConnections.map((c) => (
              <marker
                key={`marker-404040`}
                id={`arrowhead-404040`}
                markerWidth="5"
                markerHeight="10"
                refX="5"
                refY="3"
                orient="auto"
              >
                <polygon points="0 0, 6 3, 0 6" fill="#404040" />
              </marker>
            )),
          ),
        )}
      </defs>
      {/* horizontal dotted grid lines */}
      {Array.from(
        {
          length:
            Math.ceil(timelineConfig.maxTime - timelineConfig.minTime) + 2,
        },
        (_, i) => i,
      ).map((time) => (
        <line
          key={`grid-line-${time}`}
          x1="0"
          y1={topPadding + time * rowHeight}
          x2="100%"
          y2={topPadding + time * rowHeight}
          stroke="#40404060"
          strokeDasharray="2,4"
          strokeWidth="1"
          pointerEvents="none"
        />
      ))}
      {/* render all arrow paths underneath */}
      {dataFlowConnections.map((connection, idx) => {
        const source = instructionPositions.get(connection.sourceId);
        const target = instructionPositions.get(connection.targetId);

        if (!source && !target) return null;

        const props = getArrowProps(
          source,
          target,
          0,
          connection.variableName,
        );

        return (
          <ArrowPath
            key={`arrow-path-${connection.sourceId}:${connection.targetId}`}
            {...props}
          />
        );
      })}
      {/* render all arrow labels on top */}
      {dataFlowConnections.map((connection, idx) => {
        const source = instructionPositions.get(connection.sourceId);
        const target = instructionPositions.get(connection.targetId);

        if (!source && !target) return null;

        const props = getArrowProps(
          source!,
          target!,
          0,
          connection.variableName,
        );

        return (
          <ArrowLabel
            key={`arrow-label-${connection.sourceId}:${connection.targetId}`}
            {...props}
          />
        );
      })}
    </svg>
  );
};
