import * as React from "react";
import ReactTable from "react-table";
import { AppContext, IAppContext, nInSeparator } from "../../app/AppContext";
import { SynthesisDecisionView, Refactor } from "../../../gen/types";

type History = [string, SynthesisDecisionView<string, string, string, string>];

type HistoryProps = {
  history: History[];
};

export const HistoryTableView: React.FC<HistoryProps> = ({ history }) => {
  const appContext = React.useContext(AppContext) as IAppContext;
  const style = {
    fontWeight: 600,
  };

  return (
    <div className="columns">
      <Table
        name="History"
        history={history}
        columns={[
          numberColumn(),
          nidColumn(appContext.selectNode),
          textColumn("tag", (h: History) => h[1].tag, 100),
          textColumn("description", (h: History) => {
            let desc: string | Refactor<string, string> = "";
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

  function numberColumn() {
    return {
      maxWidth: 30,
      Cell: (row: any) => {
        return <div>{row.index + 1}</div>;
      }
    };
  }

  function nidColumn(onUpdateNid: (nid: string) => void) {
    return {
      Header: "nid",
      maxWidth: 30,
      Cell: (row: { original: History }) => {
        let nid: string[] = row.original[0].split(nInSeparator);
        return (
          <button className="btn-link bg-transparent p-0 border-0" onClick={() => onUpdateNid(row.original[0])}>
            {nid[nid.length - 1]}>
          </button>
        );
      }
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
      Cell: (row: { original: History }) => f(row.original)
    };
  }
};
