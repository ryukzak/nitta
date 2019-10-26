import * as React from "react";
import "react-table/react-table.css";
import { haskellApiService } from "../../../services/HaskellApiService";
import { IntermediateView } from "./IntermadiateView";
import { JsonView } from "./JsonView";
import { AppContext, IAppContext } from "../../app/AppContext";

export interface INodeViewProps {}

export interface INodeViewState {
  synthesisNode: any;
}

export const NodeView: React.FC<INodeViewProps> = props => {
  const appContext = React.useContext(AppContext) as IAppContext;

  const [synthesisNodeData, setSynthesisNodeData] = React.useState<any | null>(null);
  React.useEffect(() => {
    haskellApiService
      .getNode(appContext.selectedNodeId)
      .then((response: any) => setSynthesisNodeData(response.data))
      .catch((err: any) => console.log(err));
  }, [appContext.selectedNodeId]);

  return (
    <>
      {appContext.selectedNodeId ? (
        synthesisNodeData ? (
          <div className="m-3">
            <div className="d-flex flex-row">
              <div className="edgeGraphContainer">
                <IntermediateView selectedNId={appContext.selectedNodeId} />
              </div>
              <div className="overflow-auto">
                <JsonView src={synthesisNodeData} />
              </div>
            </div>
          </div>
        ) : (
          <pre> updating... </pre>
        )
      ) : (
        <pre> synthesis is not selected </pre>
      )}
    </>
  );
};
