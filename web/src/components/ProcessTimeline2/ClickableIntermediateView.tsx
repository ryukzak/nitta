import {type FC, useEffect, useRef} from "react";
import {IIntermediateViewProps, IntermediateView} from "../IntermediateView";

export type IClickableIntermediateViewProps = IIntermediateViewProps & {
  onToggle: (label: string) => void
};


export const ClickableIntermediateView: FC<IClickableIntermediateViewProps> = (props) => {
  const containerRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (!props.onToggle) return;
    const container = containerRef.current;
    if (!container) return;

    const observer = new MutationObserver(() => {
      const svg = container.querySelector('svg');
      if (!svg) return;

      const nodes = svg.querySelectorAll('.node');

      nodes.forEach(node => {
        if ((node as any).__clickBound) return;
        (node as any).__clickBound = true;

        const link = node.querySelector('a');
        const tooltip = link?.getAttribute('title');
        if (!tooltip) return

        (node as HTMLElement).style.cursor = 'pointer';

        node.addEventListener('click', () => {
          props.onToggle(tooltip)
        });
      });
    });

    observer.observe(container, {childList: true, subtree: true});

    return () => observer.disconnect();
  }, []);


  return (
    <div ref={containerRef}>
      <IntermediateView {...props}/>
    </div>
  );
}


