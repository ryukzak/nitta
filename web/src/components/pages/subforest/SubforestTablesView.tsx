import * as React from "react";
import ReactTable, { Column } from "react-table";
import * as Icon from "react-bootstrap-icons";

import { AppContext, IAppContext } from "components/app/AppContext";
import { Node, Bind, Dataflow, EndpointDecision, Target } from "services/HaskellApiService";
import { BreakLoop, OptimizeAccum, ResolveDeadlock } from "services/HaskellApiService";
import { BindMetrics, DataflowMetrics, FView } from "gen/types";

import { sidColumn, textColumn, objectiveColumn, decisionColumn, parametersColumn, detailColumn } from "./Columns";

// FIXME: Type hell. There should be a nicer way to organize this whole thing.

type EdgesProps = {
  nodes: Node[];
};

export const SubforestTablesView: React.FC<EdgesProps> = ({ nodes }) => {
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
        nodes={nodes.filter((e: Node) => e.decision.tag === "BindDecisionView")}
        columns={[
          sidColumn(appContext.setSID),
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
        nodes={nodes.filter((e) => e.decision.tag !== "DataflowDecisionView" && e.decision.tag !== "BindDecisionView")}
        columns={[
          sidColumn(appContext.setSID),
          objectiveColumn(),
          textColumn("type", (e: Node) => e.decision.tag, 160),
          textColumn("description", (e: Node) => {
            if (e.decision.tag === "BreakLoopView") {
              let d = e.decision as BreakLoop;
              return "output: " + d.outputs.join(", ") + " input: " + d.input;
            } else if (e.decision.tag === "OptimizeAccumView") {
              let d = e.decision as OptimizeAccum;
              return (
                <pre>
                  {d.old.map((e: FView) => e.fvFun).join("\n")}
                  <br />
                  <Icon.ArrowDown />
                  <br />
                  {d.new.map((e: FView) => e.fvFun).join(", ")}
                  <br />
                </pre>
              );
            } else if (e.decision.tag === "ResolveDeadlockView") {
              return (e.decision as ResolveDeadlock).newBuffer;
            }
            return JSON.stringify(e.decision);
          }),
          detailColumn(),
        ]}
      />
      <Table
        name="Dataflow"
        nodes={nodes.filter((e: Node) => e.decision.tag === "DataflowDecisionView")}
        columns={[
          sidColumn(appContext.setSID),
          objectiveColumn(),
          textColumn("source", (e: Node) => (e.decision as Dataflow).source[0], 60),
          textColumn(
            "targets",
            (e: Node) => {
              let targets = (e.decision as Dataflow).targets;
              return (
                <div>
                  {targets.map((target: [string, EndpointDecision], i: number) => (
                    <pre key={i}>
                      {(target[1].epRole as Target).contents} &rarr; {target[0]} @ {target[1].epAt[0]} ...{" "}
                      {target[1].epAt[1]}
                    </pre>
                  ))}
                </div>
              );
            },
            undefined,
            true
          ),
          textColumn("wait", (e: Node) => (e.parameters as DataflowMetrics).pWaitTime, 60),
          textColumn(
            "not transferable input",
            (e: Node) => JSON.stringify((e.parameters as DataflowMetrics).pNotTransferableInputs),
            60
          ),
          textColumn("restricted", (e: Node) => String((e.parameters as DataflowMetrics).pRestrictedTime), 60),
          detailColumn(),
        ]}
      />
      <Table
        name="Other"
        nodes={nodes.filter((e: Node) => known.indexOf(e.decision.tag) === -1)}
        columns={[
          sidColumn(appContext.setSID),
          objectiveColumn(),
          decisionColumn(),
          parametersColumn(),
          detailColumn(),
        ]}
      />
    </>
  );

  // FIXME: shouldn't it be in Table.tsx?
  function Table(props: { name: string; columns: Column[]; nodes: Node[] }) {
    if (props.nodes.length === 0)
      return (
        <small>
          <pre style={style}>{props.name}: NOTHING</pre>
        </small>
      );
    return (
      <small style={style}>
        <pre>{props.name}</pre>
        <ReactTable
          defaultPageSize={props.nodes.length}
          minRows={props.nodes.length}
          showPagination={false}
          columns={props.columns}
          data={props.nodes}
        />
        <br />
      </small>
    );
  }
};
