import * as React from "react";
import { Button } from "react-bootstrap";
import { EdgeView, Interval } from "../../../gen/types";

type Edge = EdgeView<string, string, number, number>;

const style = {
  fontWeight: 600,
}

export function nidColumn(separator: string, onUpdateNid: (nid: string) => void) {
  return {
    Header: "nid",
    maxWidth: 30,
    Cell: (row: { original: Edge }) => {
      let nid: string[] = row.original.nid.split(separator);
      return (
        <Button
          variant="link"
          className="btn btn-link bg-transparent p-0  border-0"
          style={style}
          onClick={() => onUpdateNid(row.original.nid)}
        >
          {nid[nid.length - 1]}>
        </Button>
      );
    }
  };
}

export function decisionColumn() {
  return {
    Header: "decision",
    style: style,
    Cell: (row: { original: Edge }) => JSON.stringify(row.original.decision),
  };
}

export function textColumn(
  columnName: string,
  f: (e: Edge) => string | number | Interval<number> | React.ReactElement,
  maxWidth?: number,
  wrap?: boolean
) {
  let textColStyle = style;
  if (wrap) textColStyle = {...style, ...{whiteSpace: "unset"}};

  return {
    Header: columnName,
    style: textColStyle,
    maxWidth: maxWidth,
    Cell: (row: { original: Edge }) => f(row.original),
  };
}

export function parametersColumn() {
  return {
    Header: "parameters",
    style: style,
    Cell: (row: { original: Edge }) => JSON.stringify(row.original.parameters),
  };
}

export function objectiveColumn() {
  return {
    Header: "Z(d)",
    maxWidth: 40,
    style: style,
    Cell: (row: { original: Edge }) => row.original.objectiveFunctionValue,
  };
}
