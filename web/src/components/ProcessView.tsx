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
        return <pre key={i}>{" ".repeat(n)}{v} => {points.map(this.renderPoint)}</pre>;
    }

    renderPoint(point: TimelinePoint<number>[], i: number) {
        let s: string = "#";
        if (point.length === 0) {
            s = ".";
        }
        if (point.length === 1) {
            s = "*";
        }
        const click = () => {this.setState({detail: point})};
        return <span key={i} onClick={click}>{s}</span>;
    }

    render() {
        if (!this.state.data) {
            return <pre>LOADING</pre>;
        }
        let viewPointLength: number = 0;
        this.state.data.timelines.forEach(e => {
            let l: number = this.viewpoint2string(e[0]).length;
            if (l > viewPointLength) {
                viewPointLength = l;
            }
        });
        return <div className="row">
                <div className="columns large-8">
                    {this.state.data.timelines.map(
                        (e, i) => {
                            return this.renderLine(i, viewPointLength, e[0], e[1]);
                        })}
                </div>
                <pre className="tiny columns large-4">
                    {JSON.stringify(this.state.detail, null, 2)}
                </pre>
            </div>;
    }
}
