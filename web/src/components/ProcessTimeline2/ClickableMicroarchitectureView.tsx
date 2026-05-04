import { type FC, useEffect, useRef } from "react";
import type {
  DataFlow,
  IMicroarchitectureViewProps,
} from "../MicroarchitectureView";
import { MicroarchitectureView } from "../MicroarchitectureView";

export type IClickableMicroarchitectureViewProps =
  IMicroarchitectureViewProps & {
    onToggle: (unitName: string) => void;
    selectedInstructionId?: number | null;
    selectedDataFlowId?: string | null;
    onClearSelection?: () => void;
  };

export const ClickableMicroarchitectureView: FC<
  IClickableMicroarchitectureViewProps
> = (props) => {
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
        let tooltip = link?.getAttribute("title");
        tooltip = tooltip?.split("_").pop();
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
        currentElement.id?.startsWith("node") ||
        currentElement.classList.contains("node") ||
        currentElement.tagName === "G"
      ) {
        const titleInNode = currentElement.querySelector("title");
        if (titleInNode?.textContent?.includes("::")) {
          isOnNode = true;
          break;
        }
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
      aria-label="Microarchitecture view"
    >
      <MicroarchitectureView
        unitColors={props.unitColors}
        enabledUnits={props.enabledUnits}
        highligthedUnits={props.highligthedUnits}
        highlightedDataFlows={props.highlightedDataFlows}
      />
    </section>
  );
};
