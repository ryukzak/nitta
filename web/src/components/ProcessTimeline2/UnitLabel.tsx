import type React from "react";
import type { FC } from "react";
import type { Color } from "../../utils/color";
import "components/ProcessTimeline2/LegendItem.scss";

interface UnitLabelProps {
  componentName: string;
  color: Color;
  enabled?: boolean;
  onToggle?: (componentName: string) => void;
}

export const UnitLabel: FC<UnitLabelProps> = ({
  componentName,
  color,
  enabled = true,
  onToggle,
}) => {
  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Enter" || e.key === " ") {
      e.preventDefault();
      onToggle?.(componentName);
    }
  };

  const labelContent = (
    <>
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
    </>
  );

  if (onToggle) {
    return (
      <button
        type="button"
        className={`legend-item ${!enabled ? "disabled" : ""}`}
        onClick={() => onToggle(componentName)}
        onKeyDown={handleKeyDown}
        style={{ cursor: "pointer" }}
      >
        {labelContent}
      </button>
    );
  }

  return (
    <div
      className={`legend-item ${!enabled ? "disabled" : ""}`}
      style={{ cursor: "default" }}
    >
      {labelContent}
    </div>
  );
};
