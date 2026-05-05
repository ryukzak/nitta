import { useLayoutEffect } from "react";

/**
 * Custom hook to handle wheel scaling and shift+wheel horizontal scrolling
 * Prevents parent container scrolling while allowing timeline zoom and pan
 */
export const useWheelScale = (
  containerRef: React.RefObject<HTMLDivElement | null>,
  onScaleChange: (delta: number) => void,
): void => {
  useLayoutEffect(() => {
    const container = containerRef.current;
    if (!container) return;

    const handleWheelNative = (e: WheelEvent) => {
      e.preventDefault();
      e.stopPropagation();

      if (e.shiftKey) {
        // Shift + wheel scroll for horizontal scrolling
        container.scrollLeft += e.deltaY;
      } else {
        // Regular wheel scroll for scaling
        const delta = e.deltaY > 0 ? 0.9 : 1.1;
        onScaleChange(delta);
      }
    };

    container.addEventListener("wheel", handleWheelNative, { passive: false });
    return () => {
      container.removeEventListener("wheel", handleWheelNative);
    };
  }, [containerRef, onScaleChange]);
};
