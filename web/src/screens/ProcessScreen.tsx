import Axios from "axios";
import React, { useEffect, useState, useContext, FC } from "react";

import { api } from "services/HaskellApiService";
import { ProcessTimelines, TimelinePoint, TimelineWithViewPoint, ViewPointID } from "services/gen/types";

import { AppContext, IAppContext } from "app/AppContext";
import { TimelineView } from "components/ProcessTimeline";
import { ProcessView } from "components/ProcessView";
import "screens/ProcessScreen.scss";
import { getDefaultAxiosErrorHandler } from "utils/axiosErrorHanders";

export interface Highlight {
  up: number[];
  current: number[];
  down: number[];
}

// FIXME: deprecated (use `GET /node/:sid/process`)
// TODO: diff from previous synthesis process step
// TODO: highlight point by click on info part
// FIXME: https://robinpokorny.medium.com/index-as-a-key-is-an-anti-pattern-e0349aece318
export const ProcessScreen: FC = () => {
  const appContext = useContext(AppContext) as IAppContext;

  const [pIdIndex, setPIdIndex] = useState<Record<number, TimelinePoint<number>> | null>(null);
  const [highlight, setHighlight] = useState<Highlight>({ up: [], current: [], down: [] } as Highlight);
  const [detail, setDetail] = useState<TimelinePoint<number>[]>([] as TimelinePoint<number>[]);
  const [data, setData] = useState<ProcessTimelines<number> | null>(null);

  useEffect(() => {
    const source = Axios.CancelToken.source();

    setDetail([]);
    setHighlight({ up: [], current: [], down: [] });
    api
      .getTimelines(appContext.selectedSID, source.token)
      .then((response: { data: ProcessTimelines<number> }) => {
        console.log("> ProcessScreen.requestTimelines - done");
        let pIdIndex: Record<number, TimelinePoint<number>> = {};
        response.data.timelines.forEach((vt) => {
          vt.timelinePoints.forEach((point) => {
            point.forEach((e) => {
              const x: number = e.pID;
              pIdIndex[x] = e;
            });
          });
        });
        let resort = resortTimeline(response.data);
        setData(resort);
        setPIdIndex(pIdIndex);
      })
      .catch(getDefaultAxiosErrorHandler());

    return () => {
      source.cancel();
    };
  }, [appContext.selectedSID]);

  if (!data) return <pre>LOADING</pre>;

  if (data.timelines.length === 0) return <pre>EMPTY PROCESS TIMELINE</pre>;

  return (
    <div className="row m-3">
      <div className="col-lg-6">
        <h3>Process Graph</h3>
        <ProcessView />
      </div>
      <div className="col-lg-6">
        <h3>Process Timeline (deprecated)</h3>
        <div className="p-3 d-flex flex-nowrap">
          <TimelineView
            timelines={data.timelines}
            highlight={highlight}
            data={data}
            onHighlightChange={(h) => setHighlight(h)}
            onDetailChange={(d) => setDetail(d)}
          />
          <div className="ml-2 flex-grow-1" style={{ minWidth: "30%" }}>
            <hr />
            <div className="squeeze upRelation">upper related:</div>
            <div className="x-scrollable">
              {highlight.up.map((e, i) =>
                pIdIndex !== null && pIdIndex[e] !== undefined ? (
                  <div key={i} className="squeeze">
                    - {pIdIndex[e].pInfo}
                  </div>
                ) : (
                  ""
                )
              )}
            </div>
            <hr />
            <div className="squeeze current">current:</div>
            <div className="x-scrollable">
              {detail.map((e) => (
                <div className="squeeze">- {e.pInfo}</div>
              ))}
            </div>
            <hr />
            <div className="squeeze downRelation">bottom related:</div>
            <div className="x-scrollable">
              {highlight.down.map((e) =>
                pIdIndex != null && pIdIndex[e] != null ? <div className="squeeze">-- {pIdIndex[e].pInfo}</div> : ""
              )}
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

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
    tmp.forEach((e) => {
      if (p(e.timelineViewpoint)) {
        result.timelines.push(e);
      } else {
        newTmp.push(e);
      }
    });
    tmp = newTmp;
  }
  function section(msg: string) {
    result.timelines.push({
      timelineViewpoint: { level: msg, component: [] },
      timelinePoints: [],
    });
  }

  section("# CADs:");
  extract((e) => e.level === "CAD");
  section("# Functions:");
  extract((e) => e.level === "Fun");
  section("# Dataflow:");
  extract((e) => e.level === "INST" && e.component.length === 0);
  extract((e) => e.level === "EndPoint");
  section("# Intructions:");
  extract((e) => e.component.length === 0);
  extract((e) => true);
  return result;
}
