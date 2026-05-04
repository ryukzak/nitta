import React, {
  type FC,
  type ReactNode,
  useCallback,
  useEffect,
  useRef,
  useState,
} from "react";
import "components/utils/SplitPane.scss";

interface SplitPaneProps {
  children: ReactNode;
  initialSplitPercentage?: number;
  minWidthLeft?: number;
  minWidthRight?: number;
  orientation?: "vertical" | "horizontal";
}

interface SplitPaneInnerProps {
  children: ReactNode;
  initialSplitPercentage: number;
  minWidthLeft: number;
  minWidthRight: number;
  orientation: "vertical" | "horizontal";
}

const SplitPaneInner: FC<SplitPaneInnerProps> = ({
  children,
  initialSplitPercentage,
  minWidthLeft,
  minWidthRight,
  orientation,
}) => {
  const childrenArray = React.Children.toArray(children);
  const [splitPercentage, setSplitPercentage] = useState(
    initialSplitPercentage,
  );
  const [isLeftHidden, setIsLeftHidden] = useState(false);
  const [isRightHidden, setIsRightHidden] = useState(false);
  const lastSplitPercentage = useRef(initialSplitPercentage);

  const containerRef = useRef<HTMLDivElement>(null);
  const isResizing = useRef(false);

  const startResizing = useCallback(
    (e: React.MouseEvent) => {
      e.preventDefault();
      isResizing.current = true;
      document.body.style.cursor =
        orientation === "horizontal" ? "row-resize" : "col-resize";
      document.body.style.userSelect = "none";
    },
    [orientation],
  );

  const stopResizing = useCallback(() => {
    isResizing.current = false;
    document.body.style.cursor = "default";
    document.body.style.userSelect = "auto";
  }, []);

  const updateSplitPercentage = useCallback(
    (newPercentage: number) => {
      const snapThreshold = 5;

      if (newPercentage <= snapThreshold) {
        setSplitPercentage(0);
        setIsLeftHidden(true);
        setIsRightHidden(false);
        return;
      }
      if (newPercentage >= 100 - snapThreshold) {
        setSplitPercentage(100);
        setIsRightHidden(true);
        setIsLeftHidden(false);
        return;
      }

      if (!containerRef.current) return;
      const containerRect = containerRef.current.getBoundingClientRect();
      const isHorizontal = orientation === "horizontal";
      const containerSize = isHorizontal
        ? containerRect.height
        : containerRect.width;
      const firstPaneSize = (newPercentage / 100) * containerSize;
      const secondPaneSize = containerSize - firstPaneSize;

      if (firstPaneSize >= minWidthLeft && secondPaneSize >= minWidthRight) {
        setSplitPercentage(newPercentage);
        setIsLeftHidden(false);
        setIsRightHidden(false);
      }
    },
    [minWidthLeft, minWidthRight, orientation],
  );

  const onMouseMove = useCallback(
    (e: MouseEvent) => {
      if (!isResizing.current || !containerRef.current) return;

      const containerRect = containerRef.current.getBoundingClientRect();
      const isHorizontal = orientation === "horizontal";
      const newPercentage = isHorizontal
        ? ((e.clientY - containerRect.top) / containerRect.height) * 100
        : ((e.clientX - containerRect.left) / containerRect.width) * 100;

      updateSplitPercentage(newPercentage);
    },
    [updateSplitPercentage, orientation],
  );

  const onKeyDown = useCallback(
    (e: React.KeyboardEvent) => {
      const step = 5;
      let newPercentage = splitPercentage;
      const isHorizontal = orientation === "horizontal";

      const increaseKey = isHorizontal ? "ArrowDown" : "ArrowRight";
      const decreaseKey = isHorizontal ? "ArrowUp" : "ArrowLeft";

      if (e.key === increaseKey) {
        e.preventDefault();
        newPercentage = Math.min(splitPercentage + step, 100);
      } else if (e.key === decreaseKey) {
        e.preventDefault();
        newPercentage = Math.max(splitPercentage - step, 0);
      }

      if (newPercentage !== splitPercentage) {
        updateSplitPercentage(newPercentage);
      }
    },
    [splitPercentage, updateSplitPercentage, orientation],
  );

  useEffect(() => {
    window.addEventListener("mousemove", onMouseMove);
    window.addEventListener("mouseup", stopResizing);
    return () => {
      window.removeEventListener("mousemove", onMouseMove);
      window.removeEventListener("mouseup", stopResizing);
    };
  }, [onMouseMove, stopResizing]);

  const isHorizontal = orientation === "horizontal";

  return (
    <div
      ref={containerRef}
      className={`split-pane-container split-pane-${orientation} ${isLeftHidden ? "left-hidden" : ""} ${isRightHidden ? "right-hidden" : ""}`}
    >
      <div
        className="split-pane-left"
        style={
          isHorizontal
            ? {
                height: isRightHidden
                  ? "100%"
                  : isLeftHidden
                    ? "0"
                    : `${splitPercentage}%`,
              }
            : {
                width: isRightHidden
                  ? "100%"
                  : isLeftHidden
                    ? "0"
                    : `${splitPercentage}%`,
              }
        }
      >
        {childrenArray[0]}
      </div>
      <div
        className="split-pane-divider"
        onMouseDown={startResizing}
        onKeyDown={onKeyDown}
        role="slider"
        tabIndex={0}
        aria-label="Split pane divider"
        aria-valuenow={Math.round(splitPercentage)}
        aria-valuemin={0}
        aria-valuemax={100}
      >
        <div className="split-pane-controls">
          <div className="divider-handle" />
        </div>
      </div>
      <div
        className="split-pane-right"
        style={
          isHorizontal
            ? {
                height: isLeftHidden
                  ? "100%"
                  : isRightHidden
                    ? "0"
                    : `${100 - splitPercentage}%`,
              }
            : {
                width: isLeftHidden
                  ? "100%"
                  : isRightHidden
                    ? "0"
                    : `${100 - splitPercentage}%`,
              }
        }
      >
        {childrenArray[1]}
      </div>
    </div>
  );
};

export const SplitPane: FC<SplitPaneProps> = ({
  children,
  initialSplitPercentage = 50,
  minWidthLeft = 0,
  minWidthRight = 0,
  orientation = "vertical",
}) => {
  const childrenArray = React.Children.toArray(children);
  if (childrenArray.length < 2) {
    return <>{children}</>;
  }

  return (
    <SplitPaneInner
      initialSplitPercentage={initialSplitPercentage}
      minWidthLeft={minWidthLeft}
      minWidthRight={minWidthRight}
      orientation={orientation}
    >
      {childrenArray}
    </SplitPaneInner>
  );
};
