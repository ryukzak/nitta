import React, { ReactElement } from "react";

import { Popover, OverlayTrigger } from "react-bootstrap";
import * as Icon from "react-bootstrap-icons";

import { Bind, Dataflow, BreakLoop, OptimizeAccum, ConstantFolding, ResolveDeadlock } from "services/HaskellApiService";
import { Node, sidSeparator, EndpointDecision, Target } from "services/HaskellApiService";
import { Interval, FView, DecisionView } from "services/gen/types";

const style = {
  fontWeight: 600,
};

export function sidColumn(onUpdateNid: (sid: string) => void) {
  return {
    Header: "sid",
    maxWidth: 30,
    Cell: (row: { original: Node }) => {
      let sid: string[] = row.original.sid.split(sidSeparator);
      return (
        <button className="btn-link bg-transparent p-0 border-0" onClick={() => onUpdateNid(row.original.sid)}>
          {sid[sid.length - 1]} {">"}
        </button>
      );
    },
  };
}

export function decisionColumn() {
  return {
    Header: "decision",
    style: style,
    Cell: (row: { original: Node }) => JSON.stringify(row.original.decision),
  };
}

export function textColumn(
  columnName: string,
  f: (e: Node) => string | number | Interval<number> | ReactElement,
  maxWidth?: number,
  wrap?: boolean
) {
  let textColStyle = style;
  if (wrap) textColStyle = { ...style, ...{ whiteSpace: "unset" } };

  return {
    Header: columnName,
    style: textColStyle,
    maxWidth: maxWidth,
    Cell: (row: { original: Node }) => f(row.original),
  };
}

export function detailColumn() {
  return textColumn(
    "",
    (e: Node) => {
      return (
        <OverlayTrigger
          trigger={["hover", "focus"]}
          key={e.sid}
          placement="left"
          overlay={
            <Popover id={`popover-positioned-left`}>
              <Popover.Title>{e.decision.tag}</Popover.Title>
              <Popover.Content>
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
                    )
                  )}
                </pre>
              </Popover.Content>
            </Popover>
          }
        >
          <Icon.InfoCircle />
        </OverlayTrigger>
      );
    },
    25
  );
}

export function parametersColumn() {
  return {
    Header: "parameters",
    style: style,
    Cell: (row: { original: Node }) => JSON.stringify(row.original.parameters),
  };
}

export function objectiveColumn() {
  return {
    Header: "Z(d)",
    maxWidth: 50,
    style: style,
    Cell: (row: { original: Node }) => row.original.score,
  };
}

export function showDecision(decision: DecisionView): ReactElement {
  if (decision.tag === "BindDecisionView") return showBind(decision);
  else if (decision.tag === "DataflowDecisionView") return showDataflow(decision);
  else if (decision.tag === "BreakLoopView") return showBreakLoop(decision);
  else if (decision.tag === "ConstantFoldingView") return showConstantFolding(decision);
  else if (decision.tag === "OptimizeAccumView") return showOptimizeAccum(decision);
  else if (decision.tag === "ResolveDeadlockView") return showResolveDeadlock(decision);
  else throw new Error("Unkown decision type: " + decision.tag);
}

export function showBind(decision: Bind): ReactElement {
  return (
    <div>
      <strong>{decision.pu}</strong> <Icon.ArrowLeft /> {decision.function.fvFun}
    </div>
  );
}

export function showDataflow(decision: Dataflow): ReactElement {
  let targets = decision.targets;
  return (
    <div>
      from: <strong>{decision.source[0]}</strong> <br />
      {targets.map((target: [string, EndpointDecision], i: number) => (
        <div key={target[0]}>
          {i + 1}) <strong>{(target[1].epRole as Target).contents}</strong> <Icon.ArrowLeft />{" "}
          <strong>{target[0]}</strong> @ {target[1].epAt[0]} ... {target[1].epAt[1]}
          <br />
        </div>
      ))}
    </div>
  );
}

export function showBreakLoop(decision: BreakLoop): ReactElement {
  return <div>{"output: " + decision.outputs.join(", ") + " input: " + decision.input}</div>;
}

export function showConstantFolding(d: ConstantFolding): ReactElement {
  return (
    <div>
      {d.cRefOld.map((e: FView) => e.fvFun).join("\n")}
      <br />
      <Icon.ArrowDown />
      <br />
      {d.cRefNew.map((e: FView) => e.fvFun).join(", ")}
      <br />
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

export function showResolveDeadlock(decision: ResolveDeadlock): ReactElement {
  return <div>{decision.newBuffer}</div>;
}
