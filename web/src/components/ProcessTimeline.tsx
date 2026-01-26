import React, { type FC } from "react";

import type { Highlight } from "screens/ProcessScreen";
import type {
  ProcessTimelines,
  TimelinePoint,
  TimelineWithViewPoint,
  ViewPointID,
} from "services/gen/types";

// FIXME: review, refactor (reorganize?)

interface TimelineProps {
  timelines: TimelineWithViewPoint<number>[];
  highlight: Highlight;
  data: ProcessTimelines<number> | null;
  onHighlightChange: (highlight: Highlight) => void;
  onDetailChange: (detail: TimelinePoint<number>[]) => void;
}

export const TimelineView: FC<TimelineProps> = ({
  timelines,
  highlight,
  data,
  onHighlightChange,
  onDetailChange,
}) => {
  const viewColumnHead = "view point";
  let viewColumnLength: number = viewColumnHead.length;
  timelines.forEach((e) => {
    const l: number = viewpoint2string(e.timelineViewpoint, 0).length;
    if (l > viewColumnLength) {
      viewColumnLength = l;
    }
  });

  function selectPoint(point: TimelinePoint<number>[]) {
    const highlight_tmp: Highlight = { up: [], current: [], down: [] };
    point.forEach((p) => {
      const id: number = p.pID;
      highlight_tmp.current.push(p.pID);
      data!.verticalRelations.forEach((e) => {
        const up = e[0],
          down = e[1];
        if (highlight_tmp.up.indexOf(up) === -1) {
          if (id === down) {
            highlight_tmp.up.push(up);
          }
        }
        if (highlight_tmp.down.indexOf(down) === -1) {
          if (id === up) {
            highlight_tmp.down.push(down);
          }
        }
      });
    });
    onDetailChange(point);
    onHighlightChange(highlight_tmp);
  }

  function renderPoint(point: TimelinePoint<number>[], i: number) {
    let s: string = ".";
    if (point.length === 1) {
      s = "*";
    }
    if (point.length > 1) {
      s = "#";
    }
    for (let j = 0; j < point.length; j++) {
      const id = point[j].pID;
      if (highlight.up.indexOf(id) >= 0) {
        return (
          <button
            key={i}
            className="upRelation"
            onClick={() => selectPoint(point)}
            type={"button"}
          >
            {s}
          </button>
        );
      }
      if (highlight.current.indexOf(id) >= 0) {
        return (
          <button
            key={i}
            className="current"
            onClick={() => selectPoint(point)}
            type={"button"}
          >
            {s}
          </button>
        );
      }
      if (highlight.down.indexOf(id) >= 0) {
        return (
          <button
            key={i}
            className="downRelation"
            onClick={() => selectPoint(point)}
            type={"button"}
          >
            {s}
          </button>
        );
      }
    }
    return (
      <button key={i} onClick={() => selectPoint(point)} type={"button"}>
        {s}
      </button>
    );
  }

  function renderLine(
    i: number,
    viewLength: number,
    view: ViewPointID,
    points: TimelinePoint<number>[][],
  ) {
    return (
      <div key={i} className="squeeze m-0">
        {viewpoint2string(view, viewLength)} | {points.map(renderPoint)}
      </div>
    );
  }

  return (
    <div
      className="m-0 p-0"
      style={{ overflow: "auto hidden", whiteSpace: "nowrap" }}
    >
      <div className="squeeze">
        <u>
          {viewColumnHead}
          {" ".repeat(viewColumnLength - viewColumnHead.length)} | timeline
        </u>
      </div>
      {timelines.map((e, i) => {
        return renderLine(
          i,
          viewColumnLength,
          e.timelineViewpoint,
          e.timelinePoints,
        );
      })}
    </div>
  );
};

function viewpoint2string(view: ViewPointID, n: number): string {
  if (view.level.startsWith("#")) {
    const s = view.level;
    return s + spaces(n - s.length);
  }
  const s = view.component + "@" + view.level;
  return spaces(n - s.length) + s;
}

function spaces(l: number): string {
  return l > 0 ? " ".repeat(l) : "";
}
