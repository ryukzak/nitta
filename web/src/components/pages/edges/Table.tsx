import * as React from "react";
import { Button } from "react-bootstrap";
import { EdgeView, Interval } from "../../../gen/types";
import { nInSeparator } from "../../app/AppContext";

type Edge = EdgeView<string, string, number, number>;

const style = {
  fontWeight: 600,
}

export function nidColumn(onUpdateNid: (nid: string) => void) {
  return {
    Header: "nid",
    maxWidth: 30,
    Cell: (row: { original: Edge }) => {
      let nid: string[] = row.original.nid.split(nInSeparator);
      return (
        <a href="#" onClick={() => onUpdateNid(row.original.nid)}>
          {nid[nid.length - 1]}>
        </a>
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
