import React, { FC, useContext, useEffect, useState } from "react";
import "react-table/react-table.css";

import { AppContext, IAppContext } from "app/AppContext";
import { IntermediateView } from "components/IntermediateView";
import { MicroarchitectureView } from "components/MicroarchitectureView";
import { api } from "services/HaskellApiService";
import { TreeInfo } from "services/gen/types";
import { AxiosResponse } from "axios";
import { JsonView } from "components/JsonView";
import { RequestStatusInfo } from "components/utils/RequestStatusInfo";

export interface INodeScreenProps {}

export const NodeScreen: FC<INodeScreenProps> = (props) => {
  const { selectedSID } = useContext(AppContext) as IAppContext;

  const [treeInfo, setTreeInfo] = useState<TreeInfo | null>(null);
  const [treeInfoErrorMessage, setTreeInfoErrorMessage] = useState<string | null>(null);
  const [treeInfoRefreshTrigger, setTreeInfoRefreshTrigger] = useState(false);
  useEffect(() => {
    setTreeInfoErrorMessage(null);
    setTreeInfo(null);
    api
      .getTreeInfo()
      .then((response: AxiosResponse<TreeInfo>) => setTreeInfo(response.data))
      .catch((err: Error) => {
        console.error(err);
        setTreeInfoErrorMessage(`Couldn't load TreeInfo: ${err.message}`);
      });
  }, [selectedSID, treeInfoRefreshTrigger]);

  if (!selectedSID) return <pre> synthesis is not selected </pre>;

  return (
    <div className="m-3">
      <h3>sid:</h3>
      <pre>{selectedSID}</pre>

      <h3>Tree info:</h3>
      {treeInfo ? (
        <JsonView src={treeInfo} />
      ) : (
        <div className="m-5">
          <RequestStatusInfo
            errorMessage={treeInfoErrorMessage}
            refreshButtonProps={{ onClick: () => setTreeInfoRefreshTrigger((v) => !v) }}
            isSpinnerCentered={false}
          />
        </div>
      )}

      <h3>Dataflow graph:</h3>
      <IntermediateView />

      <MicroarchitectureView />
    </div>
  );
};
