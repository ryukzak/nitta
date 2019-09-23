import * as React from "react";
import { haskellAPI } from "../middleware/haskell-api";
import ReactTable from "react-table";
import { IntermediateView } from "./IntermediateView";
import { SynthesisHistoryView } from "./SynthesisHistoryView";
import { EdgeView, IBindingView, IBindEdgeParameter, IRefactorView, IDataflowView, IDataFlowEdgeParameter, Interval } from "../gen/types";

type Edge = EdgeView<string, string, number, number>;
type Binding = IBindingView<string, string, number, number>;
type BindingParam = IBindEdgeParameter;
type Refactor = IRefactorView<string, string, number, number>;
type Dataflow = IDataflowView<string, string, number, Interval<number>>;
type DataflowParam = IDataFlowEdgeParameter;

const nInSeparator = "-";

interface Props {
    nid: string;
    onNidChange: (nid: string) => void;
}

interface State {
    nid: string;
    origin: EdgeView<string, string, number, number>;
    edges: EdgeView<string, string, number, number>[];
}

export class EdgesView extends React.Component<Props, State> {
    constructor(props: Props) {
        super(props);
        this.state = {
            nid: props.nid,
            origin: null,
            edges: null,
        };
    }

    static getDerivedStateFromProps(props: Props, state: State) {
        if (props.nid && props.nid !== state.nid) {
            return { nid: props.nid, origin: null, edges: null } as State;
        }
        return null;
    }

    componentDidMount() {
        this.requestEdges(this.state.nid);
    }

    componentDidUpdate(prevProps: Props, prevState: State, snapshot: any) {
        if (prevState.nid !== this.state.nid) {
            this.requestEdges(this.state.nid);
        }
    }

    requestEdges(nid: string) {
        if (nid === undefined || nid === null) return;
        haskellAPI.getEdges(nid)
            .then((response: { data: Edge[] }) => {
                this.setState({
                    edges: response.data,
                });
            })
            .catch(err => console.log(err));
        haskellAPI.getEdge(nid)
            .then((response: { data: Edge }) => {
                this.setState({
                    origin: response.data,
                });
            })
            .catch(err => console.log(err));
    }

    updateNid(nid: string) {
        this.props.onNidChange(nid);
    }

    render() {
        if (this.state.edges === undefined || this.state.edges === null) return <div />;

        /* FIXME: history and table view of decision should be similar */
        return (
            <div className="row">
                <div className="columns">
                    <Table
                        name="Binding"
                        edges={this.state.edges.filter(e => e.decision.tag === "BindingView")}
                        columns={[
                            nidColumn(this.props.onNidChange),
                            objectiveColumn(),

                            textColumn("function", (e: Edge) => (e.decision as Binding).function),
                            textColumn("pu", (e: Edge) => (e.decision as Binding).pu),

                            textColumn("crit", (e: Edge) => String((e.parameters as BindingParam).pCritical)),
                            textColumn("lock", (e: Edge) => String((e.parameters as BindingParam).pPossibleDeadlock)),
                            textColumn("wave", (e: Edge) => (e.parameters as BindingParam).pWave),
                            textColumn("outputs", (e: Edge) => (e.parameters as BindingParam).pOutputNumber),
                            textColumn("alt", (e: Edge) => (e.parameters as BindingParam).pAlternative),
                            textColumn("rest", (e: Edge) => (e.parameters as BindingParam).pRestless),

                            textColumn("newDF", (e: Edge) => (e.parameters as BindingParam).pAllowDataFlow),
                            textColumn("binded functions", (e: Edge) => (e.parameters as BindingParam).pNumberOfBindedFunctions),
                            textColumn("binded inputs %", (e: Edge) => (e.parameters as BindingParam).pPercentOfBindedInputs),
                        ]}
                        onNidChange={this.props.onNidChange} />
                    <Table
                        name="Refactor"
                        edges={this.state.edges.filter((e: Edge) => e.decision.tag === "RefactorView")}
                        columns={[
                            nidColumn(this.props.onNidChange),
                            objectiveColumn(),
                            textColumn("description", (e: Edge) => JSON.stringify((e.decision as Refactor).contents)),
                        ]}
                        onNidChange={this.props.onNidChange} />
                    <Table
                        name="Dataflow"
                        edges={this.state.edges.filter((e: Edge) => e.decision.tag === "DataflowView")}
                        columns={[
                            nidColumn(this.props.onNidChange),
                            objectiveColumn(),
                            textColumn("at", (e: Edge) => (e.decision as Dataflow).source.time),
                            textColumn("source", (e: Edge) => (e.decision as Dataflow).source.pu),
                            textColumn("targets", (e: Edge) => {
                                let targets = (e.decision as any as Dataflow).targets;
                                let lst = Object.keys(targets).map((k: string) => k + " -> " + (targets[k] ? targets[k].pu : ""));
                                return (<div>
                                    {lst.map((k: string, i: number) => <pre key={i}>{k}</pre>)}
                                </div>);
                            }, true),
                            textColumn("wait", (e: Edge) => (e.parameters as DataflowParam).pWaitTime),
                            textColumn("not transferable input", (e: Edge) => JSON.stringify((e.parameters as DataflowParam).pNotTransferableInputs)),
                            textColumn("restricted", (e: Edge) => String((e.parameters as DataflowParam).pRestrictedTime)),
                        ]}
                        onNidChange={this.props.onNidChange} />
                    <Table
                        name="Other"
                        edges={this.state.edges.filter((e: Edge) => ["BindingView", "RefactorView", "DataflowView"].indexOf(e.decision.tag) === -1)}
                        columns={[
                            nidColumn(this.props.onNidChange),
                            objectiveColumn(),
                            decisionColumn(),
                            parametersColumn(),
                        ]}
                        onNidChange={this.props.onNidChange} />
                </div>

                <div className="rows">
                    <div className="columns">
                        <pre className="squeze">history:</pre>
                    </div>
                </div>
                <div className="row">
                    <div className="large-7 columns">
                        <SynthesisHistoryView nId={ this.state.nid } reverse={ true } />
                    </div>
                    <div className="large-5 columns">
                        <IntermediateView selectedNId={ this.state.nid } view="synthesisNode" />
                    </div>
                </div>
            </div>
        );
    }
}

function Table(props: { name: string, columns: any[], edges: Edge[], onNidChange: (nid: string) => void }) {
    if (props.edges.length === 0)
        return (<small>
            <pre>{props.name}: NOTHING</pre>
        </small>);
    return (<small>
        <pre>{props.name}</pre>
        <ReactTable
            defaultPageSize={props.edges.length}
            minRows={props.edges.length}
            showPagination={false}
            columns={props.columns}
            data={props.edges} />
        <br />
    </small>);
}

function nidColumn(onUpdateNid: (nid: string) => void) {
    return {
        Header: "nid",
        maxWidth: 30,
        Cell: (row: { original: Edge }) => {
            let nid: string[] = row.original.nid.split(nInSeparator);
            return (<a onClick={() => onUpdateNid(row.original.nid)}>
                {nid[nid.length - 1]}
            </a>);
        }
    };
}

function decisionColumn() {
    return {
        Header: "decision",
        Cell: (row: { original: Edge }) => JSON.stringify(row.original.decision)
    };
}

// FIXME: any should be changed.
function textColumn(columnName: string, f: (e: Edge) => string | number | any, wrap?: boolean) {
    let style = {};
    if (wrap) style["whiteSpace"] = "unset";
    return {
        Header: columnName,
        style: style,
        Cell: (row: { original: Edge }) => f(row.original)
    };
}

function parametersColumn() {
    return {
        Header: "parameters",
        Cell: (row: { original: Edge }) => JSON.stringify(row.original.parameters)
    };
}

function objectiveColumn() {
    return {
        Header: "Z(d)",
        maxWidth: 40,
        Cell: (row: { original: Edge }) => row.original.objectiveFunctionValue
    };
}
