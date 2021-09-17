import React, { FC, useState, useEffect, useContext } from "react";
import "react-table/react-table.css";

import { api } from "services/HaskellApiService";
import { AppContext, IAppContext } from "app/AppContext";
import { JsonView } from "components/JsonView";
import Axios from "axios";
import axiosErrorExceptionHandler from "components/utils/axios_errors_handlers/AxiosErrorHander";

export interface IDebugScreenProps {}

export const DebugScreen: FC<IDebugScreenProps> = (props) => {
  const { selectedSID } = useContext(AppContext) as IAppContext;

  const [debugInfo, setDebugInfo] = useState<any | null>(null);
  useEffect(() => {
    const source = Axios.CancelToken.source();

    api
      .getDebugInfo(selectedSID, source.token)
      .then((response: any) => setDebugInfo(response.data))
      .catch((err: any) => {
        axiosErrorExceptionHandler(err);
      });
    return () => {
      source.cancel();
    };
  }, [selectedSID]);

  const [synthesisNodeData, setSynthesisNodeData] = useState<any | null>(null);
  useEffect(() => {
    const source = Axios.CancelToken.source();

    api
      .getNode(selectedSID, source.token)
      .then((response: any) => setSynthesisNodeData(response.data))
      .catch((err: any) => {
        axiosErrorExceptionHandler(err);
      });
    return () => {
      source.cancel();
    };
  }, [selectedSID]);

  return (
    <div className="m-3">
      {selectedSID ? (
        synthesisNodeData ? (
          <div className="d-flex flex-column">
            <h3>Current node</h3>
            <JsonView src={synthesisNodeData} />
            <h3>Debug info</h3>
            <JsonView src={debugInfo} />
          </div>
        ) : (
          <pre> Updating... </pre>
        )
      ) : (
        <pre> Synthesis is not selected! </pre>
      )}
    </div>
  );
};
