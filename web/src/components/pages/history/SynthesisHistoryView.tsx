import React, { useEffect, useState } from "react";
import ReactTable from "react-table";
import { haskellApiService } from "../../../services/HaskellApiService";
import { HistoryStep, Refactor } from "../../../gen/types";
import { AxiosResponse, AxiosError } from "axios";
import { AppContext, IAppContext } from "../../app/AppContext";
import { useContext } from "react";

type FirstStep = [string, { tag: ""; desc: string }];
type History = HistoryStep<string, string, string, string> | FirstStep;

export interface ISynthesisHistoryViewProps {
  reverse: boolean;
}

export const SynthesisHistoryView: React.FC<ISynthesisHistoryViewProps> = props => {
  const appContext = useContext(AppContext) as IAppContext;
  const [synthesisHistory, setHistory] = useState<History[]>();
  const style = {
    fontWeight: 600
  };

  const firstStep = ["-", { tag: "", desc: "INITIAL STATE" }] as History;
  
  useEffect(() => {
    haskellApiService
      .getHistory(appContext.selectedNodeId)
      .then((response: AxiosResponse<History[]>) => {
        if (props.reverse) setHistory(response.data.reverse().concat([firstStep]));
        else setHistory([firstStep].concat(response.data));
      })
      .catch((err: AxiosError) => console.log(err));
  }, [appContext.selectedNodeId, props.reverse]);

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
          return <>{props.reverse ? synthesisHistory!.length - (row as any).index : (row as any).index + 1}</>;
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
            {props.reverse ? synthesisHistory!.length - (row as any).index : (row as any).index + 1}
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

  if (synthesisHistory == null) return <pre>LOADING...</pre>;

  return (
    <div className="columns">
      <Table
        name="History"
        history={synthesisHistory}
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
          })
        ]}
      />
    </div>
  );
};
