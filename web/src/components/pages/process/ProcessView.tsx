import React, { useEffect, useState } from "react";
import { haskellApiService } from "../../../services/HaskellApiService";
import { ProcessTimelines, ViewPointID, TimelinePoint, TimelineWithViewPoint } from "../../../gen/types";

import "./ProcessView.scss";
import { viewpoint2string } from "../../../utils/componentUtils";
import { AppContext, IAppContext } from "../../app/AppContext";
import { useContext } from "react";
import { AxiosError } from "axios";

interface Highlight {
  up: number[];
  current: number[];
  down: number[];
}


// TODO: diff from previous synthesis process step
// TODO: highlight point by click on info part
export const ProcessView: React.FC = () => {
  const appContext = useContext(AppContext) as IAppContext;

  const [pIdIndex, setPIdIndex] = useState<Record<number, TimelinePoint<number>> | null>(null);
  const [highlight, setHighlight] = useState<Highlight>({ up: [], current: [], down: [] } as Highlight);
  const [detail, setDetail] = useState<TimelinePoint<number>[]>([] as TimelinePoint<number>[]);
  const [data, setData] = useState<ProcessTimelines<number> | null>(null);

  useEffect(() => {
    haskellApiService
      .getTimelines(appContext.selectedNodeId)
      .then((response: { data: ProcessTimelines<number> }) => {
        console.log("> ProcessView.requestTimelines - done");
        let pIdIndex: Record<number, TimelinePoint<number>> = {};
        response.data.timelines.forEach(vt => {
          vt.timelinePoints.forEach(point => {
            point.forEach(e => {
              const x: number = e.pID;
              pIdIndex[x] = e;
            });
          });
        });
        let resort = resortTimeline(response.data);
        setData(resort);
        setPIdIndex(pIdIndex);
      })
      .catch((err: AxiosError) => console.log(err));
  }, [appContext.selectedNodeId]);

  if (!data) {
    return <pre>LOADING</pre>;
  }
  if (data.timelines.length === 0) {
    return <pre>EMPTY PROCESS TIMELINE</pre>;
  }
  let viewColumnHead = "view point";
  let viewColumnLength: number = viewColumnHead.length;
  data.timelines.forEach(e => {
    let l: number = viewpoint2string(e.timelineViewpoint).length;
    if (l > viewColumnLength) {
      viewColumnLength = l;
    }
  });

  return (
    <div className="row">
      <div className="columns col-md-5 m-0 p-0">
        <pre className="squeeze m-0 p-0">
          <u>
            {viewColumnHead}
            {" ".repeat(viewColumnLength - viewColumnHead.length)} | timeline
              </u>
        </pre>
        {data.timelines.map((e, i) => {
          return renderLine(i, viewColumnLength, e.timelineViewpoint, e.timelinePoints);
        })}
      </div>
      <div className="columns col-md-7">
        <pre className="squeeze">------------------------------</pre>
        <pre className="squeeze upRelation">upper related:</pre>
        {highlight.up.map(e => (
          <pre className="squeeze">- {pIdIndex![e].pInfo}</pre>
        ))}
        <pre className="squeeze">------------------------------</pre>
        <pre className="squeeze current">current:</pre>
        {detail.map(e => (
          <pre className="squeeze">- {e.pInfo}</pre>
        ))}
        <pre className="squeeze">------------------------------</pre>
        <pre className="squeeze downRelation">bottom related:</pre>
        {highlight.down.map(e => (
          <pre className="squeeze">- {pIdIndex![e].pInfo}</pre>
        ))}
      </div>
    </div>
  );

  function resortTimeline(data: ProcessTimelines<number>) {
    let result: ProcessTimelines<number> = {
      timelines: [],
      verticalRelations: data.verticalRelations,
    };
    function cmp(a: TimelineWithViewPoint<number>, b: TimelineWithViewPoint<number>) {
      if (a.timelineViewpoint.component < b.timelineViewpoint.component) return -1;
      if (a.timelineViewpoint.component > b.timelineViewpoint.component) return 1;
      return 0;
    }
    let tmp: TimelineWithViewPoint<number>[] = data.timelines.sort(cmp);
    function extract(p: (id: ViewPointID) => boolean) {
      let newTmp: TimelineWithViewPoint<number>[] = [];
      tmp.forEach(e => {
        if (p(e.timelineViewpoint)) {
          result.timelines.push(e);
        } else {
          newTmp.push(e);
        }
      });
      tmp = newTmp;
    }
    extract(e => e.component.length === 0);
    extract(e => e.level === "CAD");
    extract(e => e.level === "Fun");
    extract(e => e.level === "EndPoint");
    extract(e => true);
    return result;
  }

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
    setDetail(point);
    setHighlight(highlight_tmp);
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

}
