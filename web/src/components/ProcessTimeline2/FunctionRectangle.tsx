import React, { type FC } from "react";
import { Color, fadeColor } from "../../utils/color";
import {
  type ProcessFunction,
  ROW_HEIGHT,
  TEXT_PADDING,
} from "../utils/ProcessTimeline2";
import "./FunctionRectangle.scss";

interface FunctionRectangleProps {
  func: ProcessFunction;
  bgColor: Color;
  topPadding: number;
  timelineConfig: {
    minTime: number;
    maxTime: number;
  };
  headerMode?: "inside" | "outside";
  showHeader?: boolean;
  showInstructions?: boolean;
  selectedInstructionId?: number | null;
  selectedDataFlowId?: string | null;
  onInstructionSelect?: (instructionId: number) => void;
  getRelatedDataFlows?: (instructionId: number) => string[];
  getRelatedInstructions?: (dataFlowId: string) => number[];
}

export const FunctionRectangle: FC<FunctionRectangleProps> = ({
  func,
  bgColor,
  topPadding,
  timelineConfig,
  headerMode = "outside",
  showHeader = true,
  showInstructions = true,
  selectedInstructionId,
  selectedDataFlowId,
  onInstructionSelect,
  getRelatedDataFlows,
  getRelatedInstructions,
}) => {
  const headerRef = React.useRef<HTMLDivElement | null>(null);
  const [headerHeight, setHeaderHeight] = React.useState<number>(0);

  const isInstructionSelected =
    selectedInstructionId !== null && selectedInstructionId !== undefined;
  const isDataFlowSelected =
    selectedDataFlowId !== null && selectedDataFlowId !== undefined;

  const getInstructionHighlightClass = (instructionId: number): string => {
    if (!isInstructionSelected && !isDataFlowSelected) return "";

    if (isInstructionSelected) {
      if (instructionId === selectedInstructionId)
        return "instruction-selected";
      if (
        getRelatedDataFlows &&
        getRelatedDataFlows(selectedInstructionId!).length > 0
      ) {
        const relatedFlows = getRelatedDataFlows(selectedInstructionId!);
        if (
          relatedFlows.some((flow) => {
            const [sourceId, targetId] = flow
              .split(":")
              .map((id) => (id === "null" ? null : parseInt(id)));
            return sourceId === instructionId || targetId === instructionId;
          })
        ) {
          return "instruction-related";
        }
      }
      return "";
    }

    if (isDataFlowSelected && getRelatedInstructions) {
      const relatedInstructions = getRelatedInstructions(selectedDataFlowId!);
      if (relatedInstructions.includes(instructionId)) {
        return "instruction-related";
      }
    }

    return "";
  };

  const bgTransparentColor = new Color({
    r: bgColor.obj.r,
    g: bgColor.obj.g,
    b: bgColor.obj.b,
  });
  bgTransparentColor.obj.a = 0x15 / 255;

  const tooltipText = `${func.component} #${func.pID} \n${func.label} \n[${func.startTime};${func.endTime}]`;

  React.useLayoutEffect(() => {
    if (headerRef.current) {
      const rect = headerRef.current.getBoundingClientRect();
      setHeaderHeight(rect.height);
    }
  }, []);

  return (
    <div
      key={func.pID}
      className={`function-rectangle`}
      title={tooltipText}
      style={{
        top:
          topPadding +
          (func.startTime - timelineConfig.minTime) * ROW_HEIGHT -
          (headerMode === "outside" && showHeader ? headerHeight : 0),
        height:
          (func.endTime - func.startTime + 1) * ROW_HEIGHT +
          (headerMode === "outside" && showHeader ? headerHeight : 0),
        left: `${func.leftPosition}px`,
        borderColor: bgColor.toHexString(),
        backgroundColor: bgTransparentColor.toHexString(),
      }}
    >
      {showHeader && (
        <div
          ref={headerRef}
          data-header-id={`func-header-${func.pID}`}
          className={`function-header ${headerMode === "inside" ? "inner" : ""}`}
          style={{
            backgroundColor: fadeColor(bgColor, 0x15 / 255).toHexString(),
            color: bgColor.toHexString(),
            width: func.instructionMaxWidth + TEXT_PADDING,
            height: `${headerMode === "inside" ? ROW_HEIGHT / 5 + "px" : "auto"}`,
          }}
        >
          <div className="function-name">
            <div>{func.label}</div>
          </div>
        </div>
      )}

      {/* instructions Container */}
      {showInstructions && (
        <div
          className="instructions-container"
          style={{ width: func.instructionMaxWidth + TEXT_PADDING }}
        >
          {func.instructions.map((instruction, index) => {
            const highlightClass = getInstructionHighlightClass(
              instruction.pID,
            );
            const handleKeyDown = (e: React.KeyboardEvent) => {
              if (e.key === "Enter" || e.key === " ") {
                e.preventDefault();
                onInstructionSelect?.(instruction.pID);
              }
            };

            return (
              <button
                key={instruction.pID}
                type="button"
                data-instruction-id={`instr-${instruction.pID}`}
                className={`instruction-rectangle ${highlightClass}`}
                onClick={() => onInstructionSelect?.(instruction.pID)}
                onKeyDown={handleKeyDown}
                style={{
                  top:
                    (instruction.startTime - func.startTime) * ROW_HEIGHT +
                    8 -
                    (headerMode === "inside" && index !== 0
                      ? ROW_HEIGHT / 5
                      : 0),
                  height: Math.max(
                    1,
                    (instruction.endTime - instruction.startTime + 1) *
                      ROW_HEIGHT -
                      20 -
                      (headerMode === "inside" && index === 0
                        ? ROW_HEIGHT / 5
                        : 0),
                  ),
                  border: `2px solid ${bgColor.toHexString()}`,
                  cursor: "pointer",
                }}
                title={`${func.component} #${instruction.pID}\n${instruction.label}\n[${instruction.startTime};${instruction.endTime}]`}
              >
                <div className="instruction-content">
                  <div className="instruction-label">
                    <strong>{instruction.label}</strong>
                  </div>
                  <div className="instruction-io-info">
                    ({Array.from(instruction.inputs).join(",")}) -&gt; (
                    {Array.from(instruction.outputs).join(",")})
                  </div>
                </div>
              </button>
            );
          })}
        </div>
      )}
    </div>
  );
};
