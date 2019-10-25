import * as React from "react";
import { ProcessView } from "./ProcessView";
import { SynthesisHistoryView } from "./SynthesisHistoryView";
import { EndpointOptions } from "./EndpointOptions";
import { AppContext, IAppContext } from "../../app/AppContext";
import { useContext } from "react";

export const ProcessPage: React.FC = () => {
  const appContext = useContext(AppContext) as IAppContext;

  if (appContext.selectedNodeId === null) return <div></div>;

  return (
    <div className="d-flex flex-row m-2">
      <div className="col-md-6">
        <ProcessView />
      </div>
      <div className="col-md-6">
        Synthesis history :
        <SynthesisHistoryView reverse={false} />
        <br />
        EndpointOptions :
        <EndpointOptions />
      </div>
    </div>
  );

}
