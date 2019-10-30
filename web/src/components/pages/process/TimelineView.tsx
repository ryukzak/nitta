import React from "react";
import { TimelineWithViewPoint, TimelinePoint, ViewPointID, ProcessTimelines } from "../../../gen/types";
import { viewpoint2string } from "../../../utils/componentUtils";
import { Highlight } from "./ProcessView";

interface TimelineProps {
  timelines: TimelineWithViewPoint<number>[];
  highlight: Highlight;
  data: ProcessTimelines<number> | null;
  onHighlightChange: (highlight: Highlight) => void;
  onDetailChange: (detail: TimelinePoint<number>[]) => void;
}

export const TimelineView: React.FC<TimelineProps> = ({
  timelines,
  highlight,
  data,
  onHighlightChange,
  onDetailChange,
}) => {
  let viewColumnHead = "view point";
  let viewColumnLength: number = viewColumnHead.length;
  timelines.forEach(e => {
    let l: number = viewpoint2string(e.timelineViewpoint).length;
    if (l > viewColumnLength) {
      viewColumnLength = l;
    }
  });

  return (
    <div className="m-0 p-0" style={{ overflowX: "auto" }}>
      <pre className="squeeze m-0 p-0">
        <u>
          {viewColumnHead}
          {" ".repeat(viewColumnLength - viewColumnHead.length)} | timeline
        </u>
      </pre>
      {timelines.map((e, i) => {
        return renderLine(i, viewColumnLength, e.timelineViewpoint, e.timelinePoints);
      })}
    </div>
  );

  function selectPoint(point: TimelinePoint<number>[]) {
    let highlight_tmp: Highlight = { up: [], current: [], down: [] };
    point.forEach(p => {
      let id: number = p.pID;
      highlight_tmp.current.push(p.pID);
      data!.verticalRelations.forEach(e => {
        let up = e[0],
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
          <span key={i} className="upRelation" onClick={() => selectPoint(point)}>
            {s}
          </span>
        );
      }
      if (highlight.current.indexOf(id) >= 0) {
        return (
          <span key={i} className="current" onClick={() => selectPoint(point)}>
            {s}
          </span>
        );
      }
      if (highlight.down.indexOf(id) >= 0) {
        return (
          <span key={i} className="downRelation" onClick={() => selectPoint(point)}>
            {s}
          </span>
        );
      }
    }
    return (
      <span key={i} onClick={() => selectPoint(point)}>
        {s}
      </span>
    );
  }

  function renderLine(i: number, viewLength: number, view: ViewPointID, points: TimelinePoint<number>[][]) {
    let v = viewpoint2string(view);
    let n = viewLength - v.length;
    return (
      <pre key={i} className="squeeze m-0">
        {" ".repeat(n)}
        {v} | {points.map(renderPoint)}
      </pre>
    );
  }
};
