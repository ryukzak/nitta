import React, { FC, useContext } from "react";
import ReactTable, { Column } from "react-table";

import { AppContext, IAppContext } from "app/AppContext";
import { Node, Dataflow } from "services/HaskellApiService";
import { BindMetrics, DataflowMetrics } from "services/gen/types";
import {
  sidColumn,
  textColumn,
  objectiveColumn,
  decisionColumn,
  parametersColumn,
  detailColumn,
  showDecision,
} from "components/SubforestTables/Columns";

type SubforestTablesProps = {
  nodes: Node[];
};

export const SubforestTables: FC<SubforestTablesProps> = ({ nodes }) => {
  const appContext = useContext(AppContext) as IAppContext;
  const style = {
    fontWeight: 600,
  };
  let known = [
    "RootView",
    "BindDecisionView",
    "GroupBindDecisionView",
    "DataflowDecisionView",
    "BreakLoopView",
    "ConstantFoldingView",
    "OptimizeAccumView",
    "ResolveDeadlockView",
  ];

  return (
    <>
      <Table
        name="Binding"
        nodes={nodes.filter((e: Node) => e.decision.tag === "BindDecisionView" || e.decision.tag === "GroupBindDecisionView")}
        columns={[
          sidColumn(appContext.setSID),
          objectiveColumn(),

          textColumn("description", (e: Node) => showDecision(e.decision)),

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
          detailColumn(),
        ]}
      />
      <Table
        name="Refactor"
        nodes={nodes.filter((e) => e.decision.tag !== "DataflowDecisionView" && e.decision.tag !== "BindDecisionView" && e.decision.tag !== "GroupBindDecisionView")}
        columns={[
          sidColumn(appContext.setSID),
          objectiveColumn(),
          textColumn("type", (e: Node) => e.decision.tag, 160),
          textColumn("description", (e: Node) => showDecision(e.decision)),
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
          textColumn("description", (e: Node) => showDecision(e.decision)),
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
