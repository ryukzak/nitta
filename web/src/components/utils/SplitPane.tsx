import React, { FC, ReactNode, useState, useCallback, useEffect, useRef } from 'react';

interface SplitPaneProps {
  children: ReactNode;
  initialSplitPercentage?: number;
  minWidthLeft?: number;
  minWidthRight?: number;
}

export const SplitPane: FC<SplitPaneProps> = ({
  children,
  initialSplitPercentage = 70,
  minWidthLeft = 200,
  minWidthRight = 200
}) => {
  const [splitPercentage, setSplitPercentage] = useState(initialSplitPercentage);
  const [isLeftHidden, setIsLeftHidden] = useState(false);
  const [isRightHidden, setIsRightHidden] = useState(false);
  const lastSplitPercentage = useRef(initialSplitPercentage);

  const containerRef = useRef<HTMLDivElement>(null);
  const isResizing = useRef(false);

  const childrenArray = React.Children.toArray(children);
  if (childrenArray.length < 2) {
    return <>{children}</>;
  }

  const startResizing = useCallback((e: React.MouseEvent) => {
    e.preventDefault();
    isResizing.current = true;
    document.body.style.cursor = 'col-resize';
    document.body.style.userSelect = 'none';
  }, []);

  const stopResizing = useCallback(() => {
    isResizing.current = false;
    document.body.style.cursor = 'default';
    document.body.style.userSelect = 'auto';
  }, []);

  const onMouseMove = useCallback((e: MouseEvent) => {
    if (!isResizing.current || !containerRef.current) return;

    const containerRect = containerRef.current.getBoundingClientRect();
    const newPercentage = ((e.clientX - containerRect.left) / containerRect.width) * 100;
    const snapThreshold = 5; // snap to hide if within 5% of edge

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

    // Check constraints
    const leftWidth = (newPercentage / 100) * containerRect.width;
    const rightWidth = containerRect.width - leftWidth;

    if (leftWidth >= minWidthLeft && rightWidth >= minWidthRight) {
      setSplitPercentage(newPercentage);
      setIsLeftHidden(false);
      setIsRightHidden(false);
    }
  }, [minWidthLeft, minWidthRight]);

  useEffect(() => {
    window.addEventListener('mousemove', onMouseMove);
    window.addEventListener('mouseup', stopResizing);
    return () => {
      window.removeEventListener('mousemove', onMouseMove);
      window.removeEventListener('mouseup', stopResizing);
    };
  }, [onMouseMove, stopResizing]);

  return (
    <div ref={containerRef} className={`split-pane-container ${isLeftHidden ? 'left-hidden' : ''} ${isRightHidden ? 'right-hidden' : ''}`}>
      <div
        className="split-pane-left"
        style={{
          width: isRightHidden ? '100%' : (isLeftHidden ? '0' : `${splitPercentage}%`),
          display: isLeftHidden ? 'none' : 'flex'
        }}
      >
        {childrenArray[0]}
      </div>
      <div className="split-pane-divider" onMouseDown={startResizing}>
        <div className="split-pane-controls">
          <div className="divider-handle" />
        </div>
      </div>
      <div
        className="split-pane-right"
        style={{
          width: isLeftHidden ? '100%' : (isRightHidden ? '0' : `${100 - splitPercentage}%`),
          display: isRightHidden ? 'none' : 'flex'
        }}
      >
        {childrenArray[1]}
      </div>
    </div>
  );
};
