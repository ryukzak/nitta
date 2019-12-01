import * as React from "react";
import { AppContext, IAppContext } from "../../app/AppContext";
import { haskellApiService } from "../../../services/HaskellApiService";
import { IntermediateView } from "../node/IntermediateView";
import { SynthesisHistoryView } from "../history/SynthesisHistoryView";
import { TablesView } from "./EdgeTablesView";
import { EdgeView } from "../../../gen/types";
import { AxiosResponse } from "axios";

type Edge = EdgeView<string, string, number, number>;

export const EdgesView: React.FC = () => {
  const appContext = React.useContext(AppContext) as IAppContext;
  const [edges, setEdges] = React.useState<Edge[] | null>(null);

  React.useEffect(() => {
    haskellApiService
      .getEdges(appContext.selectedNodeId)
      .then((response: AxiosResponse<Edge[]>) => {
        setEdges(response.data);
      })
      .catch(err => console.log(err));
  }, [appContext.selectedNodeId]);

  if (edges === undefined || edges === null) {
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
        </div>
        <div className="col-8">
          <TablesView edges={edges} />
        </div>
      </div>
      <div className="row">
        <div className="col">
          <SynthesisHistoryView reverse={true} />
        </div>
      </div>
    </div>
  );
};
