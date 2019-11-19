import * as React from "react";
import "react-table/react-table.css";
import { haskellApiService } from "../../../services/HaskellApiService";
import { AppContext, IAppContext } from "../../app/AppContext";
import { JsonView } from "../node/JsonView";

export interface IDebugViewProps {}

export const DebugView: React.FC<IDebugViewProps> = props => {
  const appContext = React.useContext(AppContext) as IAppContext;

  const [synthesisNodeData, setSynthesisNodeData] = React.useState<any | null>(null);
  React.useEffect(() => {
    haskellApiService
      .getNode(appContext.selectedNodeId)
      .then((response: any) => setSynthesisNodeData(response.data))
      .catch((err: any) => console.error(err));
  }, [appContext.selectedNodeId]);

  return (
    <div className="m-3">
      {appContext.selectedNodeId ? (
        synthesisNodeData ? (
          <div className="d-flex flex-row">
            <JsonView src={synthesisNodeData} collapseStringsAfterLength={120} />
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
