import * as React from "react";
import "react-table/react-table.css";
import { IntermediateView } from "./IntermediateView";
import { AppContext, IAppContext } from "../../app/AppContext";

export interface INodeViewProps {}

export interface INodeViewState {
  synthesisNode: any;
}

export const NodeView: React.FC<INodeViewProps> = props => {
  const appContext = React.useContext(AppContext) as IAppContext;

  return (
    <>
      {appContext.selectedNodeId ? (
        <div className="my-3">
          <IntermediateView selectedNId={appContext.selectedNodeId} />
        </div>
      ) : (
        <pre> synthesis is not selected </pre>
      )}
    </>
  );
};
