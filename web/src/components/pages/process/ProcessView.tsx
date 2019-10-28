import React, { useEffect, useState } from "react";
import { haskellApiService } from "../../../services/HaskellApiService";
import { ProcessTimelines, TimelinePoint } from "../../../gen/types";

import "./ProcessView.scss";
import { TimelineView } from "./TimelineView";
import { resortTimeline } from "../../../utils/componentUtils";
import { AppContext, IAppContext } from "../../app/AppContext";
import { useContext } from "react";
import { AxiosError } from "axios";

export interface Highlight {
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

  return (
    <div className="row">
      <TimelineView
        timelines={data.timelines}
        highlight={highlight}
        data={data}
        onHighlightChange={(h) => setHighlight(h)}
        onDetailChange={(d) => setDetail(d)}
      />
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
}
