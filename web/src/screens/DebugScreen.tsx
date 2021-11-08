import React, { FC, useState, useEffect, useContext } from "react";
import "react-table/react-table.css";

import { api } from "services/HaskellApiService";
import { AppContext, IAppContext } from "app/AppContext";
import { JsonView } from "components/JsonView";
import Axios from "axios";
import { getDefaultAxiosErrorHandler } from "utils/axiosErrorHanders";
import { getCancellingCleanupCallback } from "utils/axiosRequestCancellation";

export interface IDebugScreenProps {}

export const DebugScreen: FC<IDebugScreenProps> = (props) => {
  const { selectedSID } = useContext(AppContext) as IAppContext;

  const [debugInfo, setDebugInfo] = useState<any | null>(null);
  useEffect(() => {
    const source = Axios.CancelToken.source();

    api
      .getDebugInfo(selectedSID, source.token)
      .then((response: any) => setDebugInfo(response.data))
      .catch(getDefaultAxiosErrorHandler());

    return getCancellingCleanupCallback(source);
  }, [selectedSID]);

  const [synthesisNodeData, setSynthesisNodeData] = useState<any | null>(null);
  useEffect(() => {
    const source = Axios.CancelToken.source();

    api
      .getNode(selectedSID, source.token)
      .then((response: any) => setSynthesisNodeData(response.data))
      .catch(getDefaultAxiosErrorHandler());

    return getCancellingCleanupCallback(source);
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
