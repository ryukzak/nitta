import React, { useEffect, useState } from "react";
import { haskellApiService } from "../../../services/HaskellApiService";
import { SynthesisDecisionView } from "../../../gen/types";
import { AxiosResponse, AxiosError } from "axios";
import { AppContext, IAppContext } from "../../app/AppContext";
import { HistoryTableView } from "./HistoryTableView";
import { useContext } from "react";

export interface ISynthesisHistoryViewProps {
  reverse: boolean;
}

export const SynthesisHistoryView: React.FC<ISynthesisHistoryViewProps> = props => {
  const appContext = useContext(AppContext) as IAppContext;
  const [synthesisHistory, setHistory] = useState<[string, SynthesisDecisionView<string, string, string, string>][]>();

  useEffect(() => {
    haskellApiService
      .getHistory(appContext.selectedNodeId)
      .then((response: AxiosResponse<[string, SynthesisDecisionView<string, string, string, string>][]>) => {
        setHistory(response.data);
      })
      .catch((err: AxiosError) => console.log(err));
  }, [appContext.selectedNodeId]);

  if (synthesisHistory == null) return <pre>LOADING...</pre>;

  let history = props.reverse ? synthesisHistory.reverse() : synthesisHistory;
  return <HistoryTableView history={history} />;
};
