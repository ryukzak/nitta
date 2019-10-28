import { ViewPointID, ProcessTimelines, TimelineWithViewPoint } from "../gen/types";


export function getFormikFieldClass(name: string, touched: any, errors: any) {
  return `form-control ${touched[name] && errors[name] ? "is-invalid" : ""}`;
}

export function viewpoint2string(view: ViewPointID): string {
  return view.component + "@" + view.level;
}

export function resortTimeline(data: ProcessTimelines<number>) {
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
