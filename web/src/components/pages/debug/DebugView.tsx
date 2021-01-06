import * as React from "react";
import "react-table/react-table.css";
import { haskellApiService } from "../../../services/HaskellApiService";
import { AppContext, IAppContext } from "../../app/AppContext";
import { JsonView } from "../node/JsonView";

export interface IDebugViewProps {}

export const DebugView: React.FC<IDebugViewProps> = (props) => {
  const { selectedNodeId } = React.useContext(AppContext) as IAppContext;

  const [debugInfo, setDebugInfo] = React.useState<any | null>(null);
  React.useEffect(() => {
    haskellApiService
      .getDebugInfo(selectedNodeId)
      .then((response: any) => setDebugInfo(response.data))
      .catch((err: any) => console.error(err));
  }, [selectedNodeId]);

  const [synthesisNodeData, setSynthesisNodeData] = React.useState<any | null>(null);
  React.useEffect(() => {
    haskellApiService
      .getNode(selectedNodeId)
      .then((response: any) => setSynthesisNodeData(response.data))
      .catch((err: any) => console.error(err));
  }, [selectedNodeId]);

  return (
    <div className="m-3">
      {selectedNodeId ? (
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
