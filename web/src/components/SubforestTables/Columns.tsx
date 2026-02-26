import type { ColumnDef } from "@tanstack/react-table";
import type { ReactElement } from "react";
import { OverlayTrigger, Popover } from "react-bootstrap";
import * as Icon from "react-bootstrap-icons";
import type { DecisionView, FView, Interval } from "services/gen/types";
import {
  type Allocation,
  type BreakLoop,
  type ConstantFolding,
  type Dataflow,
  type EndpointDecision,
  type GroupBind,
  type Node,
  type OptimizeAccum,
  type OptimizeLogicalUnit,
  type ResolveDeadlock,
  type SingleBind,
  sidSeparator,
  type Target,
} from "services/HaskellApiService";
import { Color } from "utils/color";

const style = {
  fontWeight: 600,
};

const GOOD_SCORE_COLOR = Color.fromHex("#84e371");

export function sidColumn(onUpdateNid: (sid: string) => void): ColumnDef<Node> {
  return {
    header: "sid",
    minSize: 30,
    maxSize: 47,
    cell: ({ row }) => {
      const sid: string[] = row.original.sid.split(sidSeparator);
      return (
        <button
          className="btn-link bg-transparent p-0 border-0"
          onClick={() => onUpdateNid(row.original.sid)}
          type={"button"}
        >
          {sid[sid.length - 1]} {">"}
        </button>
      );
    },
  };
}

export function decisionColumn(): ColumnDef<Node> {
  return {
    header: "decision",
    cell: ({ row }) => JSON.stringify(row.original.decision),
    meta: { style },
  };
}

export function textColumn(
  columnName: string,
  f: (e: Node) => string | number | Interval<number> | ReactElement,
  maxWidth?: number,
  wrap?: boolean,
): ColumnDef<Node> {
  let textColStyle = style;
  if (wrap) textColStyle = { ...style, ...{ whiteSpace: "unset" } };

  return {
    header: columnName,
    cell: ({ row }) => <span style={textColStyle}>{f(row.original)}</span>,
  };
}

export function detailColumn(): ColumnDef<Node> {
  return textColumn(
    "detail",
    (e: Node) => {
      return (
        <OverlayTrigger
          trigger={["hover", "focus"]}
          key={e.sid}
          placement="left"
          overlay={
            <Popover id={`popover-positioned-left`}>
              <Popover.Header>{e.decision.tag}</Popover.Header>
              <Popover.Body>
                <b>Decision:</b>
                <pre>{JSON.stringify(e.decision, undefined, 2)}</pre>
                <hr />
                <b>Metrics:</b>
                <pre>
                  {Object.keys(e.parameters).map(
                    (k: string): ReactElement => (
                      <div key={e.sid}>
                        - {k}: {JSON.stringify(e.parameters[k])}
                        <br />
                      </div>
                    ),
                  )}
                </pre>
              </Popover.Body>
            </Popover>
          }
        >
          <Icon.InfoCircle />
        </OverlayTrigger>
      );
    },
    25,
  );
}

export function parametersColumn(): ColumnDef<Node> {
  return {
    header: "parameters",
    meta: { style },
    cell: ({ row }) => JSON.stringify(row.original.parameters),
  };
}

export interface ScoresInfo {
  minScore: number;
  maxScore: number;
}

export function objectiveColumn(scoresInfo: ScoresInfo): ColumnDef<Node> {
  const objectiveCellStyle = { ...style, padding: "0" };
  return {
    header: "Z(d)",
    maxSize: 20,
    meta: { objectiveCellStyle },
    cell: ({ row }) => {
      const cellColor = new Color({
        ...GOOD_SCORE_COLOR.obj,
        a:
          (row.original.score - scoresInfo.minScore) /
          (scoresInfo.maxScore - scoresInfo.minScore),
      });

      if (scoresInfo.minScore === scoresInfo.maxScore) {
        cellColor.obj.a = 1;
      }

      return (
        <OverlayTrigger
          trigger={["hover", "focus"]}
          key={row.original.sid}
          placement="left"
          overlay={
            <Popover id={`popover-positioned-left`}>
              <Popover.Header>Scores</Popover.Header>
              <Popover.Body>
                <pre>{JSON.stringify(row.original.scores, undefined, 2)}</pre>
              </Popover.Body>
            </Popover>
          }
        >
          <div
            style={{
              padding: "7px 5px",
              height: "100%",
              backgroundColor: cellColor.toRgbaString(),
            }}
          >
            {row.original.score}
          </div>
        </OverlayTrigger>
      );
    },
  };
}

export function showDecision(decision: DecisionView): ReactElement {
  if (decision.tag === "SingleBindView") return showBind(decision);
  else if (decision.tag === "GroupBindView") return showBinds(decision);
  else if (decision.tag === "DataflowDecisionView")
    return showDataflow(decision);
  else if (decision.tag === "BreakLoopView") return showBreakLoop(decision);
  else if (decision.tag === "ConstantFoldingView")
    return showConstantFolding(decision);
  else if (decision.tag === "OptimizeAccumView")
    return showOptimizeAccum(decision);
  else if (decision.tag === "OptimizeLogicalUnitView")
    return showOptimizeLogicalUnit(decision);
  else if (decision.tag === "ResolveDeadlockView")
    return showResolveDeadlock(decision);
  else if (decision.tag === "AllocationView") return showAllocation(decision);
  else throw new Error("Unkown decision type: " + decision.tag);
}

export function showBinds(decision: GroupBind): ReactElement {
  const binds = Object.keys(decision.bindGroup).map((uTag: string) => {
    const fs = decision.bindGroup[uTag]!;
    return (
      <div key={uTag}>
        <strong>{uTag}</strong> <Icon.ArrowLeft />
        <ul>
          {fs.map((e) => (
            <li key={e.fvFun}>{e.fvFun}</li>
          ))}
        </ul>
      </div>
    );
  });
  return <div> {binds} </div>;
}

export function showBind(decision: SingleBind): ReactElement {
  return (
    <div>
      <strong>{decision.pu}</strong> <Icon.ArrowLeft />{" "}
      {decision.function.fvFun}
    </div>
  );
}

export function showDataflow(decision: Dataflow): ReactElement {
  const targets = decision.targets;
  return (
    <div>
      from: <strong>{decision.source[0]}</strong> <br />
      {targets.map((target: [string, EndpointDecision], i: number) => (
        <div key={target[0]}>
          {i + 1}) <strong>{(target[1].epRole as Target).contents}</strong>{" "}
          <Icon.ArrowRight /> <strong>{target[0]}</strong> @ {target[1].epAt[0]}{" "}
          ... {target[1].epAt[1]}
          <br />
        </div>
      ))}
    </div>
  );
}

export function showBreakLoop(decision: BreakLoop): ReactElement {
  return (
    <div>
      {"output: " + decision.outputs.join(", ") + " input: " + decision.input}
    </div>
  );
}

export function showConstantFolding(d: ConstantFolding): ReactElement {
  return (
    <div>
      {d.cRefOld.map((e: FView) => (
        <>
          {" "}
          {e.fvFun} <br />
        </>
      ))}
      <Icon.ArrowDown />
      <br />
      {d.cRefNew.map((e: FView) => (
        <>
          {" "}
          {e.fvFun} <br />
        </>
      ))}
    </div>
  );
}

export function showOptimizeAccum(d: OptimizeAccum): ReactElement {
  return (
    <div>
      {d.old.map((e: FView) => e.fvFun).join("\n")}
      <br />
      <Icon.ArrowDown />
      <br />
      {d.new.map((e: FView) => e.fvFun).join(", ")}
      <br />
    </div>
  );
}

export function showOptimizeLogicalUnit(d: OptimizeLogicalUnit): ReactElement {
  return (
    <div>
      {d.lOld.map((e: FView) => e.fvFun).join("\n")}
      <br />
      <Icon.ArrowDown />
      <br />
      {d.lNew.map((e: FView) => e.fvFun).join(", ")}
      <br />
    </div>
  );
}

export function showResolveDeadlock(decision: ResolveDeadlock): ReactElement {
  return <div>{decision.newBuffer}</div>;
}

export function showAllocation(decision: Allocation): ReactElement {
  return (
    <div>
      <strong>{decision.networkTag}</strong> <Icon.ArrowLeft />{" "}
      {decision.processUnitTag}
    </div>
  );
}
