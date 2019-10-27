import * as React from "react";
import { Button } from "react-bootstrap";
import ReactTable from "react-table";
import { AppContext, IAppContext } from "../../app/AppContext";
import { haskellApiService } from "../../../services/HaskellApiService";
import { IntermediateView } from "../node/IntermediateView";
import { SynthesisHistoryView } from "../process/SynthesisHistoryView";
import { JsonResponse } from "../../../gen/types_mock";
import {
  EdgeView,
  IBindingView,
  IBindEdgeParameter,
  IRefactorView,
  IDataflowView,
  IDataFlowEdgeParameter,
  Interval,
} from "../../../gen/types";

type Edge = EdgeView<string, string, number, number>;
type Binding = IBindingView<string, string, number, number>;
type BindingParam = IBindEdgeParameter;
type Refactor = IRefactorView<string, string, number, number>;
type Dataflow = IDataflowView<string, string, number, Interval<number>>;
type DataflowParam = IDataFlowEdgeParameter;

export const EdgesView: React.FC = props => {
  const appContext = React.useContext(AppContext) as IAppContext;
  const [origin, setOrigin] = React.useState<EdgeView<string, string, number, number> | null>(null);
  const [edges, setEdges] = React.useState<EdgeView<string, string, number, number>[] | null>(null);
  const [currentNodeId, setCurrentNodeNId] = React.useState<string>("");
  const nInSeparator = "-";

  React.useEffect(() => {
    console.log(origin);
    if (currentNodeId !== appContext.selectedNodeId || currentNodeId.length === 0) {
      appContext.selectedNodeId === null ? setCurrentNodeNId("-") : setCurrentNodeNId(appContext.selectedNodeId);
          requestEdges(appContext.selectedNodeId!);
    }
  }, [appContext.selectedNodeId, currentNodeId, origin]);

  const requestEdges = (nid: string) => {
    if (nid === undefined || nid === null) return;
    haskellApiService
      .getEdges(nid)
      .then((response: { data: Edge[] }) => {
        setEdges(response.data);
      })
      .catch(err => console.log(err));
    haskellApiService
      .getEdge(nid)
      .then((response: { data: Edge }) => {
        setOrigin(response.data);
      })
      .catch(err => console.log(err));
  }
  
    if (edges === undefined || edges === null) return <div />;

    /* FIXME: history and table view of decision should be similar */
    return (
      <div className="m-3">
        <div className="row">
          <div className="p-1 mr-5">
            <IntermediateView />
          </div>
          <div className="columns">
            <Table
              name="Binding"
              edges={edges.filter(e => e.decision.tag === "BindingView")}
              columns={[
                nidColumn(appContext.selectNode),
                // this.props.onNidChange
                objectiveColumn(),

                textColumn("function", (e: Edge) => (e.decision as Binding).function),
                textColumn("pu", (e: Edge) => (e.decision as Binding).pu, 50),

                textColumn("crit", (e: Edge) => String((e.parameters as BindingParam).pCritical), 50),
                textColumn("lock", (e: Edge) => String((e.parameters as BindingParam).pPossibleDeadlock), 50),
                textColumn("wave", (e: Edge) => (e.parameters as BindingParam).pWave, 50),
                textColumn("outputs", (e: Edge) => (e.parameters as BindingParam).pOutputNumber, 70),
                textColumn("alt", (e: Edge) => (e.parameters as BindingParam).pAlternative, 50),
                textColumn("rest", (e: Edge) => (e.parameters as BindingParam).pRestless, 50),

                textColumn("newDF", (e: Edge) => (e.parameters as BindingParam).pAllowDataFlow, 70),
                textColumn("newBind", (e: Edge) => (e.parameters as BindingParam).pNumberOfBindedFunctions, 70),
                textColumn("|inputs|", (e: Edge) => (e.parameters as BindingParam).pPercentOfBindedInputs, 70),
              ]}
              onNidChange={appContext.selectNode}
            />
            <Table
              name="Refactor"
              edges={edges.filter((e: Edge) => e.decision.tag === "RefactorView")}
              columns={[
                nidColumn(appContext.selectNode),
                objectiveColumn(),
                textColumn("description", (e: Edge) => JSON.stringify((e.decision as Refactor).contents)),
              ]}
              onNidChange={appContext.selectNode}
            />
            <Table
              name="Dataflow"
              edges={edges.filter((e: Edge) => e.decision.tag === "DataflowView")}
              columns={[
                nidColumn(appContext.selectNode),
                objectiveColumn(),
                textColumn("at", (e: Edge) => (e.decision as Dataflow).source.time),
                textColumn("source", (e: Edge) => (e.decision as Dataflow).source.pu),
                textColumn(
                  "targets",
                  (e: Edge) => {
                    let targets = ((e.decision as any) as Dataflow).targets;
                    let lst = Object.keys(targets).map((k: string) => k + " -> " + (targets[k] ? targets[k].pu : ""));
                    return (
                      <div>
                        {lst.map((k: string, i: number) => (
                          <pre key={i}>{k}</pre>
                        ))}
                      </div>
                    );
                  },
                  undefined,
                  true
                ),
                textColumn("wait", (e: Edge) => (e.parameters as DataflowParam).pWaitTime),
                textColumn("not transferable input", (e: Edge) =>
                  JSON.stringify((e.parameters as DataflowParam).pNotTransferableInputs)
                ),
                textColumn("restricted", (e: Edge) => String((e.parameters as DataflowParam).pRestrictedTime)),
              ]}
              onNidChange={appContext.selectNode}
            />
            <Table
              name="Other"
              edges={edges.filter(
                (e: Edge) => ["BindingView", "RefactorView", "DataflowView"].indexOf(e.decision.tag) === -1
              )}
              columns={[nidColumn(appContext.selectNode), objectiveColumn(), decisionColumn(), parametersColumn()]}
              onNidChange={appContext.selectNode}
            />
          </div>
        </div>
        <div className="row mt-2 w-100" style={{ overflowX: "auto" }}>
          <div className="columns">
            <pre className="squeze h5">History:</pre>
            <SynthesisHistoryView nId={appContext.selectedNodeId!} reverse={true} />
          </div>
        </div>
      </div>
    );

    function Table(props: { name: string; columns: any[]; edges: Edge[]; onNidChange: (nid: string) => void }) {
      if (props.edges.length === 0)
        return (
          <small>
            <pre>{props.name}: NOTHING</pre>
          </small>
        );
      return (
        <small>
          <pre>{props.name}</pre>
          <ReactTable
            defaultPageSize={props.edges.length}
            minRows={props.edges.length}
            showPagination={false}
            columns={props.columns}
            data={props.edges}
          />
          <br />
        </small>
      );
    }
    
    function nidColumn(onUpdateNid: (nid: string) => void) {
      return {
        Header: "nid",
        maxWidth: 30,
        Cell: (row: { original: Edge }) => {
          let nid: string[] = row.original.nid.split(nInSeparator);
          return (
            <Button className="btn btn-link bg-transparent p-0  border-0" onClick={() => onUpdateNid(row.original.nid)}>
              {nid[nid.length - 1]}>
            </Button>
          );
        },
      };
    }
    
    function decisionColumn() {
      return {
        Header: "decision",
        Cell: (row: { original: Edge }) => JSON.stringify(row.original.decision),
      };
    }
    
    // FIXME: any should be changed.
    function textColumn(columnName: string, f: (e: Edge) => string | number | any, maxWidth?: number, wrap?: boolean) {
      let style: JsonResponse = {};
      if (wrap) style["whiteSpace"] = "unset";
      return {
        Header: columnName,
        style: style,
        maxWidth: maxWidth,
        Cell: (row: { original: Edge }) => f(row.original),
      };
    }
    
    function parametersColumn() {
      return {
        Header: "parameters",
        Cell: (row: { original: Edge }) => JSON.stringify(row.original.parameters),
      };
    }
    
    function objectiveColumn() {
      return {
        Header: "Z(d)",
        maxWidth: 40,
        Cell: (row: { original: Edge }) => row.original.objectiveFunctionValue,
      };
    }
    
  }
