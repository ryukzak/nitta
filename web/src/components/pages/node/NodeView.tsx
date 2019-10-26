import * as React from "react";
import "react-table/react-table.css";
import { IntermediateView } from "./IntermediateView";
import { AppContext, IAppContext } from "../../app/AppContext";

export interface INodeViewProps {}

export interface INodeViewState {
  synthesisNode: any;
}

export const NodeView: React.FC<INodeViewProps> = props => {
  const { selectedNodeId } = React.useContext(AppContext) as IAppContext;

  return (
    <>
      {selectedNodeId ? (
        <div className="my-3">
          <IntermediateView />
        </div>
      ) : (
        <pre> synthesis is not selected </pre>
      )}
    </>
  );
};
