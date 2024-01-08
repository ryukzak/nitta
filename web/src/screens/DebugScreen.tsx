import React, { FC, useContext, useEffect, useState } from "react";
import "react-table/react-table.css";

import { AppContext, IAppContext } from "app/AppContext";
import { JsonView } from "components/JsonView";
import { api } from "services/HaskellApiService";

export interface IDebugScreenProps {}

export const DebugScreen: FC<IDebugScreenProps> = (props) => {
  const { selectedSid } = useContext(AppContext) as IAppContext;

  const [debugInfo, setDebugInfo] = useState<any | null>(null);
  useEffect(() => {
    api
      .getDebugInfo(selectedSid)
      .then((response: any) => setDebugInfo(response.data))
      .catch((err: any) => console.error(err));
  }, [selectedSid]);

  const [synthesisNodeData, setSynthesisNodeData] = useState<any | null>(null);
  useEffect(() => {
    api
      .getNode(selectedSid)
      .then((response: any) => setSynthesisNodeData(response.data))
      .catch((err: any) => console.error(err));
  }, [selectedSid]);

  return (
    <div className="m-3">
      {selectedSid ? (
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
