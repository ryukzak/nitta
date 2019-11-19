import React, { useEffect, useState } from "react";
import { haskellApiService } from "../../../services/HaskellApiService";
import { SynthesisDecisionView } from "../../../gen/types";
import { AxiosResponse, AxiosError } from "axios";
import { AppContext, IAppContext } from "../../app/AppContext";
import { useContext } from "react";

// FIXME: review, refactor

export interface ISynthesisHistoryViewProps {
  reverse: boolean;
}

export const SynthesisHistoryView: React.FC<ISynthesisHistoryViewProps> = props => {
  const appContext = useContext(AppContext) as IAppContext;

  const [synthesisHistory, setHistory] = useState<SynthesisDecisionView<string, string, string, string>[]>();

  useEffect(() => {
    haskellApiService
      .getHistory(appContext.selectedNodeId)
      .then((response: AxiosResponse<SynthesisDecisionView<string, string, string, string>[]>) => {
        setHistory(response.data);
      })
      .catch((err: AxiosError) => console.log(err));
  }, [appContext.selectedNodeId]);

  if (synthesisHistory == null) return <pre>LOADING...</pre>;

  let history = props.reverse ? synthesisHistory.reverse() : synthesisHistory;
  // FIXME: history decisions should be view as in edges view
  return (
    <pre className="squeeze">
      {" "}
      {history.map((el: SynthesisDecisionView<string, string, string, string>, i: number) => (
        <div key={i}>
          {" "}
          {(props.reverse ? history.length - i - 1 : i) + " - "}
          {el.tag === "BindingView" && el.tag + " | " + el.pu + " <- " + el.function}
          {el.tag === "RefactorView" && el.tag + " - " + el.contents}
          {el.tag === "DataflowView" && el.tag + " - " + JSON.stringify(el)}
        </div>
      ))}{" "}
    </pre>
  );
};
