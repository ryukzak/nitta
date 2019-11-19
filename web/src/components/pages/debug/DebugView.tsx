import * as React from "react";
import "react-table/react-table.css";
import { haskellApiService } from "../../../services/HaskellApiService";
import { AppContext, IAppContext } from "../../app/AppContext";
import { JsonView } from "../node/JsonView";

export interface IDebugViewProps {}

export const DebugView: React.FC<IDebugViewProps> = props => {
  const { selectedNodeId } = React.useContext(AppContext) as IAppContext;

  const [debugData, setDebugData] = React.useState<any | null>(null);
  const [synthesisNodeData, setSynthesisNodeData] = React.useState<any | null>(null);

  // TODO: generalize/leave what's needed
  React.useEffect(() => {
    haskellApiService
      .getNode(selectedNodeId)
      .then((response: any) => setSynthesisNodeData(response.data))
      .catch((err: any) => console.error(err));
  }, [selectedNodeId]);

  React.useEffect(() => {
    haskellApiService
      .getDebugOptions(selectedNodeId)
      .then((response: any) => setDebugData(response.data))
      .catch((err: any) => console.error(err));
  }, [selectedNodeId]);

  return (
    <div className="m-3">
      {selectedNodeId ? (
        synthesisNodeData ? (
          <div className="d-flex flex-column">
            <JsonView src={debugData} />
            <JsonView src={synthesisNodeData} />
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
