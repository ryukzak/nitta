import React, { type FC } from 'react';
import { Color } from '../../utils/color';
import "components/ProcessTimeline2/LegendItem.scss";

interface LegendItemProps {
  componentName: string;
  color: Color;
  enabled?: boolean;
  onToggle?: (componentName: string) => void;
}

export const LegendItem: FC<LegendItemProps> = ({ componentName, color, enabled = true, onToggle }) => {
  return (
    <div
      className={`legend-item ${!enabled ? 'disabled' : ''}`}
      onClick={() => onToggle?.(componentName)}
      style={{ cursor: onToggle ? 'pointer' : 'default' }}
    >
      <div
        className="legend-color-swatch"
        style={{
          backgroundColor: color.toHexString(),
          opacity: enabled ? 1 : 0.4,
        }}
      />
      <span className="legend-label" style={{ opacity: enabled ? 1 : 0.4 }}>
        {componentName}
      </span>
    </div>
  );
};
