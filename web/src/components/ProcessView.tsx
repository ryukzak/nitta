import * as React from "react";
import { haskellAPI } from "../middleware/haskell-api";
import { ProcessTimelines, ViewPointID, TimelinePoint, TimelineWithViewPoint } from "../gen/types";

interface ProcessViewProps {
    nId: string;
}

interface ProcessViewState {
    nId: string;
    data: ProcessTimelines<number>;
    pIdIndex: Record<number, TimelinePoint<number>>;
    detail: TimelinePoint<number>[];
    highlight: Highlight;
}

interface Highlight {
    up: number[];
    current: number[];
    down: number[];
}

export class ProcessView extends React.Component<ProcessViewProps, ProcessViewState> {
    // TODO: diff from previous synthesis process step
    // TODO: highlight point by click on info part
    state: ProcessViewState = {
        nId: null,
        data: null,
        pIdIndex: null,
        detail: [],
        highlight: {
            up: [],
            current: [],
            down: [],
        },
    };

    constructor(props) {
        super(props);
        this.requestTimelines = this.requestTimelines.bind(this);
        this.renderPoint = this.renderPoint.bind(this);
        this.selectPoint = this.selectPoint.bind(this);
    }

    static getDerivedStateFromProps(props: ProcessViewProps, state: ProcessViewState) {
        console.log("> ProcessView.getDerivedStateFromProps", props.nId);
        if (props.nId && props.nId !== state.nId) {
            console.log("> ProcessView.getDerivedStateFromProps - new state");
            return { nId: props.nId, data: null } as ProcessViewState;
        }
        return null;
    }

    componentDidMount() {
        console.log("> ProcessView.componentDidMount", this.state.nId);
        this.requestTimelines(this.state.nId);
    }

    componentDidUpdate(prevProps: ProcessViewProps, prevState: ProcessViewState, snapshot: any) {
        console.log("> ProcessView.componentDidUpdate");
        if (prevState.nId !== this.state.nId) {
            this.requestTimelines(this.state.nId);
        }
    }

    requestTimelines(nId: string) {
        console.log("> ProcessView.requestTimelines");
        haskellAPI.getTimelines(nId)
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
                this.setState({
                    data: this.resortTimeline(response.data),
                    pIdIndex: pIdIndex,
                });
            })
            .catch((err: any) => console.log(err));
    }

    resortTimeline(data: ProcessTimelines<number>) {
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

    viewpoint2string(view: ViewPointID): string {
        return view.component + "@" + view.level;
    }

    renderLine(i: number, viewLength: number, view: ViewPointID, points: TimelinePoint<number>[][]) {
        let v = this.viewpoint2string(view);
        let n = viewLength - v.length;
        return <pre key={i} className="squeeze">{" ".repeat(n)}{v} | {points.map(this.renderPoint)}</pre>;
    }

    renderPoint(point: TimelinePoint<number>[], i: number) {
        let s: string = ".";
        if (point.length === 1) {
            s = "*";
        }
        if (point.length > 1) {
            s = "#";
        }
        for (let j = 0; j < point.length; j++) {
            const id = point[j].pID;
            if (this.state.highlight.up.indexOf(id) >= 0) {
                return <span key={i} className="upRelation" onClick={() => this.selectPoint(point)}>{s}</span>;
            }
            if (this.state.highlight.current.indexOf(id) >= 0) {
                return <span key={i} className="current" onClick={() => this.selectPoint(point)}>{s}</span>;
            }
            if (this.state.highlight.down.indexOf(id) >= 0) {
                return <span key={i} className="downRelation" onClick={() => this.selectPoint(point)}>{s}</span>;
            }
        }
        return <span key={i} onClick={() => this.selectPoint(point)}>{s}</span>;
    }

    selectPoint(point: TimelinePoint<number>[]) {
        let up: number[] = [];
        let current: number[] = [];
        let down: number[] = [];
        point.forEach(p => {
            let id: number = p.pID;
            current.push(id);
            this.state.data.verticalRelations.forEach(e => {
                if (up.indexOf(id) === -1) {
                    if (e[1] === id) { up.push(e[0]); }
                }
                if (down.indexOf(id) === -1) {
                    if (e[0] === id) { down.push(e[1]); }
                }
            });
        });
        this.setState({
            detail: point,
            highlight: {
                up: up,
                current: current,
                down: down,
            },
        });
    }

    render() {
        if (!this.state.data) {
            return <pre>LOADING</pre>;
        }
        if (this.state.data.timelines.length === 0) {
            return <pre>EMPTY PROCESS TIMELINE</pre>;
        }
        let viewColumnHead = "view point";
        let viewColumnLength: number = viewColumnHead.length;
        this.state.data.timelines.forEach(e => {
            let l: number = this.viewpoint2string(e.timelineViewpoint).length;
            if (l > viewColumnLength) {
                viewColumnLength = l;
            }
        });
        return <div className="row">
            <div className="columns large-7">
                <pre className="squeeze"><u>{viewColumnHead}{" ".repeat(viewColumnLength - viewColumnHead.length)} | timeline</u></pre>
                {this.state.data.timelines.map(
                    (e, i) => {
                        return this.renderLine(i, viewColumnLength, e.timelineViewpoint, e.timelinePoints);
                    })}
            </div>
            <div className="columns large-5">
                <pre className="squeeze">------------------------------</pre>
                <pre className="squeeze upRelation">upper related:</pre>
                {this.state.highlight.up.map(e => <pre className="squeeze">- {this.state.pIdIndex[e].pInfo}</pre>)}
                <pre className="squeeze">------------------------------</pre>
                <pre className="squeeze current">current:</pre>
                {this.state.detail.map(e => <pre className="squeeze">- {e.pInfo}</pre>)}
                <pre className="squeeze">------------------------------</pre>
                <pre className="squeeze downRelation">bottom related:</pre>
                {this.state.highlight.down.map(e => <pre className="squeeze">- {this.state.pIdIndex[e].pInfo}</pre>)}
            </div>
        </div>;
    }
}
