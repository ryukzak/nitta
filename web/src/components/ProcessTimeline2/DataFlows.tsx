import React, { type FC } from "react";
import {
  DataFlowConnection, ROW_HEIGHT,
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
  dataFlowConnections?: DataFlowConnection[];
  instructionPositions?: Map<number, InstructionPosition>;
  selectedInstructionId?: number | null;
  selectedDataFlowId?: string | null;
  onDataFlowSelect?: (dataFlowId: string) => void;
  getRelatedInstructions?: (dataFlowId: string) => number[];
}

export const DataFlowOverlay: FC<DataFlowOverlayProps> = ({
  topPadding,
  timelineConfig,
  dataFlowConnections = [],
  instructionPositions = new Map(),
  selectedInstructionId,
  selectedDataFlowId,
  onDataFlowSelect,
  getRelatedInstructions,
}) => {
  const getDataFlowHighlightClass = (sourceId: number | null, targetId: number | null): string => {
    const dataFlowId = `${sourceId}:${targetId}`;

    if (selectedInstructionId !== null && selectedInstructionId !== undefined &&
      (sourceId === selectedInstructionId || targetId === selectedInstructionId)) {
        return 'dataflow-related';
    }

    if (selectedDataFlowId !== null && selectedDataFlowId !== undefined && dataFlowId === selectedDataFlowId) {
        return 'dataflow-selected';
    }
    return '';
  };

  const getFillColor = (c: DataFlowConnection): string => {
    const highlightClass = getDataFlowHighlightClass(c.sourceId, c.targetId);
    return (highlightClass === 'dataflow-selected' || highlightClass === 'dataflow-related') ? "#000000" : "#404040";
  }

  return (
    <svg className="data-flow-overlay" role={"presentation"} style={{ width: '100%', height: '100%' }}>
      <defs>
        {Array.from(
          new Set(
            dataFlowConnections.map((c) => (
              <marker
                key={`arrowhead-${c.sourceId}-${c.targetId}`}
                id={`arrowhead-${c.variableName}`}
                markerWidth="5"
                markerHeight="10"
                refX="5"
                refY="3"
                orient="auto"
              >
                <polygon points="0 0, 6 3, 0 6" fill={getFillColor(c)} />
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
          y1={topPadding + time * ROW_HEIGHT}
          x2="100%"
          y2={topPadding + time * ROW_HEIGHT}
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

        const highlightClass = getDataFlowHighlightClass(connection.sourceId, connection.targetId);
        const dataFlowId = `${connection.sourceId}:${connection.targetId}`;

        return (
          <g
            key={`arrow-path-${dataFlowId}`}
            className={`dataflow-group ${highlightClass}`}
            onClick={() => onDataFlowSelect?.(dataFlowId)}
            style={{ cursor: 'pointer' }}
          >
            <ArrowPath {...props} />
          </g>
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

        const highlightClass = getDataFlowHighlightClass(connection.sourceId, connection.targetId);
        const dataFlowId = `${connection.sourceId}:${connection.targetId}`;

        return (
          <g
            key={`arrow-label-${dataFlowId}`}
            className={`dataflow-group ${highlightClass}`}
            onClick={() => onDataFlowSelect?.(dataFlowId)}
            style={{ cursor: 'pointer' }}
          >
            <ArrowLabel {...props} />
          </g>
        );
      })}
    </svg>
  );
};
