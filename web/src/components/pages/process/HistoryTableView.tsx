import * as React from "react";
import ReactTable from "react-table";
import { AppContext, IAppContext } from "../../app/AppContext";
import { HistoryStep, Refactor } from "../../../gen/types";

type FirstStep = [string, { tag: ""; desc: string }];
type History = HistoryStep<string, string, string, string> | FirstStep;

type HistoryProps = {
  history: History[];
  reverse: boolean;
};

export const HistoryTableView: React.FC<HistoryProps> = ({ history, reverse }) => {
  const appContext = React.useContext(AppContext) as IAppContext;
  const style = {
    fontWeight: 600,
  };

  const firstStep = ["-", { tag: "", desc: "INITIAL STATE" }] as History;
  history.map(e => console.log("History arr: " + e[0]));
  reverse ? (history = history.concat([firstStep])) : (history = [firstStep].concat(history));

  return (
    <div className="columns">
      <Table
        name="History"
        history={history}
        columns={[
          stepColumn(appContext.selectNode),
          textColumn("decision type", (h: History) => h[1].tag, 100),
          textColumn(" description  ", (h: History) => {
            let desc: string | Refactor<string, string> = "";
            if (h[1].tag === "") desc = h[1].desc;
            if (h[1].tag === "BindingView") desc = h[1].pu + " <- " + h[1].function;
            if (h[1].tag === "RefactorView") desc = h[1].contents;
            if (h[1].tag === "DataflowView") desc = JSON.stringify(h[1]);
            return <div>{desc}</div>;
          }),
        ]}
      />
    </div>
  );

  function Table(props: { name: string; columns: any[]; history: History[] }) {
    if (props.history.length === 0)
      return (
        <small>
          <pre style={style}>{props.name}: NOTHING</pre>
        </small>
      );
    return (
      <small style={style}>
        <pre className="squeze h5">{props.name}</pre>
        <ReactTable
          defaultPageSize={props.history.length}
          minRows={props.history.length}
          showPagination={false}
          columns={props.columns}
          data={props.history}
        />
        <br />
      </small>
    );
  }

  function stepColumn(onUpdateNid: (nid: string) => void) {
    return {
      Header: "step",
      maxWidth: 40,
      Cell: (row: { original: History }) => {
        if (
          Object.values(row.original[0])
            .map(String)
            .join("") === appContext.selectedNodeId
        )
          return <>{reverse ? history.length - (row as any).index : (row as any).index + 1}</>;
        return (
          <button
            className="btn-link bg-transparent p-0 border-0"
            onClick={() =>
              onUpdateNid(
                Object.values(row.original[0])
                  .map(String)
                  .join("")
              )
            }
          >
            {reverse ? history.length - (row as any).index : (row as any).index + 1}
          </button>
        );
      },
    };
  }

  function textColumn(
    columnName: string,
    f: (h: History) => string | React.ReactElement,
    maxWidth?: number,
    minWidth?: number
  ) {
    return {
      Header: columnName,
      style: style,
      maxWidth: maxWidth,
      minWidth: minWidth,
      Cell: (row: { original: History }) => f(row.original),
    };
  }
};
