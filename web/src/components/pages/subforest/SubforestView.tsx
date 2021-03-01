import * as React from "react";
import { AxiosResponse } from "axios";

import { api, Node } from "services/HaskellApiService";

import { AppContext, IAppContext } from "components/app/AppContext";
import { IntermediateView } from "components/pages/node/IntermediateView";
import { SynthesisHistoryView } from "components/pages/history/SynthesisHistoryView";
import { SubforestTablesView } from "./SubforestTablesView";
import { MicroarchitectureView } from "components/pages/microarchitecture/microarchitecture";

export const SubforestView: React.FC = () => {
  const appContext = React.useContext(AppContext) as IAppContext;
  const [subforest, setSubforest] = React.useState<Node[] | null>(null);

  React.useEffect(() => {
    api
      .getSubforest(appContext.selectedSID)
      .then((response: AxiosResponse<Node[]>) => {
        setSubforest(response.data);
      })
      .catch((err) => console.log(err));
  }, [appContext.selectedSID]);

  if (subforest === undefined || subforest === null) {
    return (
      <div className="m-3 text-black-50">
        <h5>Empty EdgesView</h5>
      </div>
    );
  }

  return (
    <div className="m-3">
      <div className="row">
        <div className="col-4">
          <IntermediateView />
          <MicroarchitectureView />
        </div>
        <div className="col-8">
          <SubforestTablesView nodes={subforest} />
        </div>
      </div>
      <div className="row mt-1">
        <div className="col">
          <SynthesisHistoryView reverse={true} />
        </div>
      </div>
    </div>
  );
};
