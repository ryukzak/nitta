import React, { useEffect, useState } from "react";
import { haskellApiService } from "../../../services/HaskellApiService";
import { HistoryStep } from "../../../gen/types";
import { AxiosResponse, AxiosError } from "axios";
import { AppContext, IAppContext } from "../../app/AppContext";
import { HistoryTableView } from "./HistoryTableView";
import { useContext } from "react";

export interface ISynthesisHistoryViewProps {
  reverse: boolean;
}

export const SynthesisHistoryView: React.FC<ISynthesisHistoryViewProps> = props => {
  const appContext = useContext(AppContext) as IAppContext;
  const [synthesisHistory, setHistory] = useState<HistoryStep<string, string, string, string>[]>();
  useEffect(() => {
    haskellApiService
      .getHistory(appContext.selectedNodeId)
      .then((response: AxiosResponse<HistoryStep<string, string, string, string>[]>) => {
        props.reverse ? setHistory(response.data.reverse()) : setHistory(response.data);
        setHistory(response.data);
      })
      .catch((err: AxiosError) => console.log(err));
  }, [appContext.selectedNodeId, props.reverse]);

  if (synthesisHistory == null) return <pre>LOADING...</pre>;
  return <HistoryTableView history={synthesisHistory} reverse={props.reverse} />;
};
