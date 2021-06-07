import React, { FC, useCallback, useContext } from "react";
import "react-table/react-table.css";

import { AppContext, IAppContext } from "app/AppContext";
import { IntermediateView } from "components/IntermediateView";
import { MicroarchitectureView } from "components/MicroarchitectureView";
import { api } from "services/HaskellApiService";
import { JsonView } from "components/JsonView";
import { RequestStatusInfo } from "components/utils/RequestStatusInfo";
import { useApiRequest } from "hooks/useApiRequest";

export interface INodeScreenProps {}

export const NodeScreen: FC<INodeScreenProps> = (props) => {
  const { selectedSID } = useContext(AppContext) as IAppContext;
  const treeInfoRequest = useApiRequest({
    requester: useCallback(() => {
      return api.getTreeInfo();
      // getTreeInfo result depends on selectedSID on server side, thus need to re-request the result when it's changed
      // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [selectedSID]),
  });

  if (!selectedSID) return <pre> synthesis is not selected </pre>;

  return (
    <div className="m-3">
      <h3>sid:</h3>
      <pre>{selectedSID}</pre>

      <h3>Tree info:</h3>
      {treeInfoRequest.response?.data ? (
        <JsonView src={treeInfoRequest.response.data} />
      ) : (
        <div className="m-5">
          <RequestStatusInfo
            errorMessage={treeInfoRequest.errorMessage}
            refreshButtonProps={{ onClick: treeInfoRequest.refreshRequestData }}
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
