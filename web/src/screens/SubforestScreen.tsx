import { AxiosResponse } from "axios";
import React, { FC, useContext, useEffect, useState } from "react";

import { api, Node } from "services/HaskellApiService";

import { AppContext, IAppContext } from "app/AppContext";
import { IntermediateView } from "components/IntermediateView";
import { MicroarchitectureView } from "components/MicroarchitectureView";
import { SubforestTables } from "components/SubforestTables";
import { SynthesisHistory } from "components/SynthesisHistory";

export const SubforestScreen: FC = () => {
  const appContext = useContext(AppContext) as IAppContext;
  const [subforest, setSubforest] = useState<Node[] | null>(null);

  useEffect(() => {
    api
      .getSubforest(appContext.selectedSid)
      .then((response: AxiosResponse<Node[]>) => {
        setSubforest(response.data);
      })
      .catch((err) => console.log(err));
  }, [appContext.selectedSid]);

  if (!subforest) {
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
          <SubforestTables nodes={subforest} />
        </div>
      </div>
      <div className="row mt-1">
        <div className="col">
          <SynthesisHistory reverse={true} />
        </div>
      </div>
    </div>
  );
};
