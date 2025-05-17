import React, { FC, useContext, useMemo } from "react";
import ReactTable, { Column } from "react-table";

import { AppContext, IAppContext } from "app/AppContext";
import {
  ScoresInfo,
  decisionColumn,
  detailColumn,
  objectiveColumn,
  parametersColumn,
  showDecision,
  sidColumn,
  textColumn,
} from "components/SubforestTables/Columns";
import { Dataflow, Node } from "services/HaskellApiService";
import { AllocationMetrics, DataflowMetrics, IGroupBindMetrics, ISingleBindMetrics } from "services/gen/types";

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
    "GroupBindView",
    "AllocationView",
    "SingleBindView",
    "DataflowDecisionView",
    "BreakLoopView",
    "ConstantFoldingView",
    "OptimizeAccumView",
    "OptimizeLutView",
    "ResolveDeadlockView",
  ];

  const scoresInfo = useMemo<ScoresInfo>(() => {
    const scores = nodes.map((n) => n.score);
    return {
      minScore: Math.min(...scores),
      maxScore: Math.max(...scores),
    };
  }, [nodes]);

  return (
    <>
      <Table
        name="GroupBinding"
        nodes={nodes.filter((e) => ["GroupBindView"].includes(e.decision.tag))}
        columns={[
          sidColumn(appContext.setSid),
          objectiveColumn(scoresInfo),
          textColumn("type", (e: Node) => e.decision.tag, 160),
          textColumn("description", (e: Node) => showDecision(e.decision)),

          textColumn("obvious", (e: Node) => String((e.parameters as IGroupBindMetrics).pOnlyObviousBinds), 75),
          textColumn("percent", (e: Node) => String((e.parameters as IGroupBindMetrics).pFunctionPercentInBinds), 75),
          textColumn("avg", (e: Node) => String((e.parameters as IGroupBindMetrics).pAvgBinds), 50),
          textColumn("variance", (e: Node) => String((e.parameters as IGroupBindMetrics).pVarianceBinds), 75),
          textColumn("avgLoad", (e: Node) => String((e.parameters as IGroupBindMetrics).pAvgUnitWorkload), 75),
          textColumn(
            "varianceLoad",
            (e: Node) => String((e.parameters as IGroupBindMetrics).pVarianceUnitWorkload),
            100,
          ),

          detailColumn(),
        ]}
      />
      <Table
        name="Binding"
        nodes={nodes.filter((e: Node) => e.decision.tag === "SingleBindView")}
        columns={[
          sidColumn(appContext.setSid),
          objectiveColumn(scoresInfo),

          textColumn("description", (e: Node) => showDecision(e.decision)),

          textColumn("crit", (e: Node) => String((e.parameters as ISingleBindMetrics).pCritical), 50),
          textColumn("lock", (e: Node) => String((e.parameters as ISingleBindMetrics).pPossibleDeadlock), 50),
          textColumn(
            "wave",
            (e: Node) => {
              let x = (e.parameters as ISingleBindMetrics).pWave;
              return x === undefined || x === null ? "null" : (x as number).toString();
            },
            50,
          ),
          textColumn("outputs", (e: Node) => (e.parameters as ISingleBindMetrics).pOutputNumber, 70),
          textColumn("alt", (e: Node) => (e.parameters as ISingleBindMetrics).pAlternative, 50),
          textColumn("rest", (e: Node) => (e.parameters as ISingleBindMetrics).pRestless, 50),

          textColumn("newDF", (e: Node) => (e.parameters as ISingleBindMetrics).pAllowDataFlow, 70),
          textColumn("newBind", (e: Node) => (e.parameters as ISingleBindMetrics).pNumberOfBoundFunctions, 70),
          textColumn("|inputs|", (e: Node) => (e.parameters as ISingleBindMetrics).pPercentOfBoundInputs, 70),
          detailColumn(),
        ]}
      />
      <Table
        name="Refactor"
        nodes={nodes.filter((e) =>
          [
            "BreakLoopView",
            "ConstantFoldingView",
            "AllocationView",
            "ResolveDeadlockView",
            "OptimizeAccumView",
            "OptimizeLutView",
          ].includes(e.decision.tag),
        )}
        columns={[
          sidColumn(appContext.setSid),
          objectiveColumn(scoresInfo),
          textColumn("type", (e: Node) => e.decision.tag, 160),
          textColumn("description", (e: Node) => showDecision(e.decision)),
          detailColumn(),
        ]}
      />
      <Table
        name="Allocation"
        nodes={nodes.filter((e) => e.decision.tag === "AllocationView")}
        columns={[
          sidColumn(appContext.setSid),
          objectiveColumn(scoresInfo),
          textColumn("description", (e: Node) => showDecision(e.decision)),
          textColumn("parallelism", (e: Node) => (e.parameters as AllocationMetrics).mParallelism, 200),
          textColumn("related remains", (e: Node) => (e.parameters as AllocationMetrics).mRelatedRemains, 200),
          textColumn("max parallels", (e: Node) => (e.parameters as AllocationMetrics).mMaxParallels, 200),
          textColumn("avg parallels", (e: Node) => (e.parameters as AllocationMetrics).mAvgParallels, 200),
          textColumn("min PUs for remains", (e: Node) => (e.parameters as AllocationMetrics).mMinPusForRemains, 200),
          detailColumn(),
        ]}
      />
      <Table
        name="Dataflow"
        nodes={nodes.filter((e: Node) => e.decision.tag === "DataflowDecisionView")}
        columns={[
          sidColumn(appContext.setSid),
          objectiveColumn(scoresInfo),
          textColumn("source", (e: Node) => (e.decision as Dataflow).source[0], 60),
          textColumn("description", (e: Node) => showDecision(e.decision)),
          textColumn("wait", (e: Node) => (e.parameters as DataflowMetrics).pWaitTime, 60),
          textColumn(
            "not transferable input",
            (e: Node) => JSON.stringify((e.parameters as DataflowMetrics).pNotTransferableInputs),
            60,
          ),
          textColumn("restricted", (e: Node) => String((e.parameters as DataflowMetrics).pRestrictedTime), 60),
          textColumn(
            "first wave of target use",
            (e: Node) => String((e.parameters as DataflowMetrics).pFirstWaveOfTargetUse),
            60,
          ),
          detailColumn(),
        ]}
      />
      <Table
        name="Other"
        nodes={nodes.filter((e: Node) => known.indexOf(e.decision.tag) === -1)}
        columns={[
          sidColumn(appContext.setSid),
          objectiveColumn(scoresInfo),
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
