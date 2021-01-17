import * as React from "react";
import ReactTable, { Column } from "react-table";
import { nidColumn, textColumn, objectiveColumn, decisionColumn, parametersColumn } from "./Table";
import { AppContext, IAppContext } from "../../app/AppContext";
import {
  NodeView,
  DecisionView,
  IRootView,
  IBindDecisionView,
  IDataflowDecisionView,
  BindMetrics,
  DataflowMetrics,
} from "../../../gen/types";

// FIXME: Type hell. There should be a nicer way to organize this whole thing.

type Node = NodeView<string, string, number, number>;
type Decision = DecisionView;

type Root = IRootView;
type Bind = IBindDecisionView;
type Dataflow = IDataflowDecisionView;

type EdgesProps = {
  edges: Node[];
};

export const TablesView: React.FC<EdgesProps> = ({ edges }) => {
  const appContext = React.useContext(AppContext) as IAppContext;
  const style = {
    fontWeight: 600,
  };
  let known = [
    "RootView",
    "BindDecisionView",
    "DataflowDecisionView",
    "BreakLoopView",
    "OptimizeAccumView",
    "ResolveDeadlockView",
  ];
  return (
    <>
      <Table
        name="Binding"
        edges={edges.filter((e: Node) => e.decision.tag === "BindDecisionView")}
        columns={[
          nidColumn(appContext.selectNode),
          objectiveColumn(),

          textColumn("function", (e: Node) => (e.decision as Bind).function.fvFun),
          textColumn("pu", (e: Node) => (e.decision as Bind).pu, 50),

          textColumn("crit", (e: Node) => String((e.parameters as BindMetrics).pCritical), 50),
          textColumn("lock", (e: Node) => String((e.parameters as BindMetrics).pPossibleDeadlock), 50),
          textColumn(
            "wave",
            (e: Node) => {
              let x = (e.parameters as BindMetrics).pWave;
              return x === undefined || x === null ? "null" : (x as number).toString();
            },
            50
          ),
          textColumn("outputs", (e: Node) => (e.parameters as BindMetrics).pOutputNumber, 70),
          textColumn("alt", (e: Node) => (e.parameters as BindMetrics).pAlternative, 50),
          textColumn("rest", (e: Node) => (e.parameters as BindMetrics).pRestless, 50),

          textColumn("newDF", (e: Node) => (e.parameters as BindMetrics).pAllowDataFlow, 70),
          textColumn("newBind", (e: Node) => (e.parameters as BindMetrics).pNumberOfBindedFunctions, 70),
          textColumn("|inputs|", (e: Node) => (e.parameters as BindMetrics).pPercentOfBindedInputs, 70),
        ]}
      />
      <Table
        name="Refactor"
        edges={edges.filter((e) => e.decision.tag !== "DataflowDecisionView" && e.decision.tag !== "BindDecisionView")}
        columns={[
          nidColumn(appContext.selectNode),
          objectiveColumn(),
          textColumn("description", (e: Node) => JSON.stringify(e.decision)),
          textColumn("parameters", (e: Node) => JSON.stringify(e.parameters), 50),
          textColumn(
            "pNStepBackRepeated",
            (e: Node) => {
              let n = e.parameters.pNStepBackRepeated;
              return n === undefined || n === null ? "null" : (n as number).toString();
            },
            50
          ),
        ]}
      />
      <Table
        name="Dataflow"
        edges={edges.filter((e: Node) => e.decision.tag === "DataflowDecisionView")}
        columns={[
          nidColumn(appContext.selectNode),
          objectiveColumn(),
          // textColumn("at", (e: Node) => (e.decision as Dataflow).source.time),
          textColumn("source", (e: Node) => (e.decision as Dataflow).source),
          textColumn(
            "targets",
            (e: Node) => {
              let targets = (e.decision as Dataflow).targets;
              let lst = Object.keys(targets).map((k: string) => k + " -> " + (targets[k] ? targets[k][0] : ""));
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
          textColumn("wait", (e: Node) => (e.parameters as DataflowMetrics).pWaitTime),
          textColumn("not transferable input", (e: Node) =>
            JSON.stringify((e.parameters as DataflowMetrics).pNotTransferableInputs)
          ),
          textColumn("restricted", (e: Node) => String((e.parameters as DataflowMetrics).pRestrictedTime)),
        ]}
      />
      <Table
        name="Other"
        edges={edges.filter((e: Node) => known.indexOf(e.decision.tag) === -1)}
        columns={[nidColumn(appContext.selectNode), objectiveColumn(), decisionColumn(), parametersColumn()]}
      />
    </>
  );

  // FIXME: shouldn't it be in Table.tsx?
  function Table(props: { name: string; columns: Column[]; edges: Node[] }) {
    if (props.edges.length === 0)
      return (
        <small>
          <pre style={style}>{props.name}: NOTHING</pre>
        </small>
      );
    return (
      <small style={style}>
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
};
