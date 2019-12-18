import * as React from "react";
import ReactTable, { Column } from "react-table";
import { nidColumn, textColumn, objectiveColumn, decisionColumn, parametersColumn } from "./Table";
import { AppContext, IAppContext } from "../../app/AppContext";
import {
  EdgeView,
  IBindingView,
  IBindEdgeParameter,
  IRefactorView,
  IDataflowView,
  IDataFlowEdgeParameter,
  Interval,
  IRefactorEdgeParameter
} from "../../../gen/types";

// FIXME: Type hell. There should be a nicer way to organize this whole thing.

type Edge = EdgeView<string, string, number, number>;
type Binding = IBindingView<string, string, number, number>;
type BindingParam = IBindEdgeParameter;
type Refactor = IRefactorView<string, string, number, number>;
type Dataflow = IDataflowView<string, string, number, Interval<number>>;
type DataflowParam = IDataFlowEdgeParameter;
type RefactorParam = IRefactorEdgeParameter;

type EdgesProps = {
  edges: Edge[];
};

export const TablesView: React.FC<EdgesProps> = ({ edges }) => {
  const appContext = React.useContext(AppContext) as IAppContext;
  const style = {
    fontWeight: 600,
  };

  return (
    <>
      <Table
        name="Binding"
        edges={edges.filter(e => e.decision.tag === "BindingView")}
        columns={[
          nidColumn(appContext.selectNode),
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
      />
      <Table
        name="Refactor"
        edges={edges.filter((e: Edge) => e.decision.tag === "RefactorView")}
        columns={[
          nidColumn(appContext.selectNode),
          objectiveColumn(),
          textColumn("description", (e: Edge) => JSON.stringify((e.decision as Refactor).contents)),
          textColumn("pVarsCount", (e: Edge) => (e.parameters as RefactorParam).pVarsCount, 50),
          textColumn("pBufferCount", (e: Edge) => (e.parameters as RefactorParam).pBufferCount, 50),
          textColumn("pNStepBackRepeated", (e: Edge) => {
            let n = (e.parameters as RefactorParam).pNStepBackRepeated;
            return n === undefined || n === null ? "null" : (n as number).toString()
          }, 50)
        ]}
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
      />
      <Table
        name="Other"
        edges={edges.filter(
          (e: Edge) => ["BindingView", "RefactorView", "DataflowView"].indexOf(e.decision.tag) === -1
        )}
        columns={[nidColumn(appContext.selectNode), objectiveColumn(), decisionColumn(), parametersColumn()]}
      />
    </>
  );

  // FIXME: shouldn't it be in Table.tsx?
  function Table(props: { name: string; columns: Column[]; edges: Edge[] }) {
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
