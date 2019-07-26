import * as React from "react";
import { haskellAPI } from "../middleware/haskell-api";
import { ProcessTimelines, ViewPointID, TimelinePoint } from "../gen/types";

interface ProcessViewProps {
    nId: string;
}

interface ProcessViewState {
    nId: string;
    data: ProcessTimelines<number>;
    detail: TimelinePoint<number>[];
}

export class ProcessView extends React.Component<ProcessViewProps, ProcessViewState> {
    state: ProcessViewState = { nId: null, data: null, detail: null };

    constructor(props) {
        super(props);
        this.renderPoint = this.renderPoint.bind(this);
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
            .then((response) => {
                console.log("> ProcessView.requestTimelines - done");
                this.setState({
                    data: response.data
                });
            })
            .catch((err: any) => console.log(err));
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
        let s: string = "#";
        if (point.length === 0) {
            s = ".";
        }
        if (point.length === 1) {
            s = "*";
        }
        return <span key={i} onClick={() => this.setState({ detail: point })}>{s}</span>;
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
            let l: number = this.viewpoint2string(e[0]).length;
            if (l > viewColumnLength) {
                viewColumnLength = l;
            }
        });
        return <div className="row">
            <div className="columns large-8">
                <pre className="squeeze"><u>{viewColumnHead}{" ".repeat(viewColumnLength - viewColumnHead.length)} | timeline</u></pre>
                {this.state.data.timelines.map(
                    (e, i) => {
                        return this.renderLine(i, viewColumnLength, e[0], e[1]);
                    })}
            </div>
            <pre className="squeeze columns large-4">
                {JSON.stringify(this.state.detail, null, 2)}
            </pre>
        </div>;
    }
}
