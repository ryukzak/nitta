import * as React from "react";

import { Popover, OverlayTrigger } from "react-bootstrap";
import * as Icon from "react-bootstrap-icons";

import { sidSeparator } from "components/app/AppContext";
import { Node } from "services/HaskellApiService";
import { Interval } from "gen/types";

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
  f: (e: Node) => string | number | Interval<number> | React.ReactElement,
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
          trigger="click"
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
                <pre>{JSON.stringify(e.parameters, undefined, 2)}</pre>
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
