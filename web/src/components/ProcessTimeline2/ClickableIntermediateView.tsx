import { type FC, useEffect, useRef } from "react";
import {
  type IIntermediateViewProps,
  IntermediateView,
} from "../IntermediateView";

export type IClickableIntermediateViewProps = IIntermediateViewProps & {
  onToggle: (label: string) => void;
  highlightedFunctions?: Set<string>;
  highlightedDataFlows?: Set<string>;
  selectedInstructionId?: number | null;
  selectedDataFlowId?: string | null;
  onClearSelection?: () => void;
};

export const ClickableIntermediateView: FC<IClickableIntermediateViewProps> = (
  props,
) => {
  const containerRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (!props.onToggle) return;
    const container = containerRef.current;
    if (!container) return;

    const observer = new MutationObserver(() => {
      const svg = container.querySelector("svg");
      if (!svg) return;

      const nodes = svg.querySelectorAll(".node");

      nodes.forEach((node) => {
        if ((node as any).__clickBound) return;
        (node as any).__clickBound = true;

        const link = node.querySelector("a");
        const tooltip = link?.getAttribute("title");
        if (!tooltip) return;

        (node as HTMLElement).style.cursor = "pointer";

        node.addEventListener("click", () => {
          props.onToggle(tooltip);
        });
      });
    });

    observer.observe(container, { childList: true, subtree: true });

    return () => observer.disconnect();
  }, [props.onToggle]);

  const handleContainerClick = (e: React.MouseEvent<HTMLDivElement>) => {
    const target = e.target as HTMLElement;

    let isOnNode = false;
    let currentElement: Element | null = target;

    while (currentElement && currentElement !== containerRef.current) {
      if (
        currentElement.classList.contains("node") ||
        currentElement.classList.contains("edge") ||
        currentElement.tagName === "A"
      ) {
        isOnNode = true;
        break;
      }
      currentElement = currentElement.parentElement;
    }

    if (!isOnNode) {
      props.onClearSelection?.();
    }
  };

  const handleContainerKeyDown = (e: React.KeyboardEvent<HTMLDivElement>) => {
    if (e.key === "Escape") {
      props.onClearSelection?.();
    }
  };

  return (
    <section
      ref={containerRef}
      onClick={handleContainerClick}
      onKeyDown={handleContainerKeyDown}
      tabIndex={-1}
      aria-label="Intermediate view"
    >
      <IntermediateView
        functionToUnitMapping={props.functionToUnitMapping}
        unitColors={props.unitColors}
        enabledFunctions={props.enabledFunctions}
        highlightedFunctions={props.highlightedFunctions}
        highlightedDataFlows={props.highlightedDataFlows}
      />
    </section>
  );
};
