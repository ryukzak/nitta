import * as React from "react";
import { Button } from "react-bootstrap";
import ReactTable from "react-table";
import { TextColumnStyle } from "../../../gen/types_mock";
import { EdgeView } from "../../../gen/types";

type Edge = EdgeView<string, string, number, number>;

const nInSeparator = "-";

export function Table(props: { name: string; columns: any[]; edges: Edge[]; onNidChange: (nid: string) => void }) {
  if (props.edges.length === 0)
    return (
      <small>
        <pre>{props.name}: NOTHING</pre>
      </small>
    );
  return (
    <small>
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

export function nidColumn(onUpdateNid: (nid: string) => void) {
  return {
    Header: "nid",
    maxWidth: 30,
    Cell: (row: { original: Edge }) => {
      let nid: string[] = row.original.nid.split(nInSeparator);
      return (
        <Button
          variant="link"
          className="btn btn-link bg-transparent p-0  border-0"
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
    Cell: (row: { original: Edge }) => JSON.stringify(row.original.decision),
  };
}

// FIXME: any should be changed.
export function textColumn(
  columnName: string,
  f: (e: Edge) => string | number | any,
  maxWidth?: number,
  wrap?: boolean
) {
  let style: TextColumnStyle = { whiteSpace: "" };
  if (wrap) style.whiteSpace = "unset";
  return {
    Header: columnName,
    style: style,
    maxWidth: maxWidth,
    Cell: (row: { original: Edge }) => f(row.original),
  };
}

export function parametersColumn() {
  return {
    Header: "parameters",
    Cell: (row: { original: Edge }) => JSON.stringify(row.original.parameters),
  };
}

export function objectiveColumn() {
  return {
    Header: "Z(d)",
    maxWidth: 40,
    Cell: (row: { original: Edge }) => row.original.objectiveFunctionValue,
  };
}
