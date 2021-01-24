import * as React from "react";
import "react-table/react-table.css";

import { AppContext, IAppContext } from "components/app/AppContext";
import { IntermediateView } from "./IntermediateView";

export interface INodeViewProps {}

export const NodeView: React.FC<INodeViewProps> = (props) => {
  const { selectedSID } = React.useContext(AppContext) as IAppContext;

  return (
    <>
      {selectedSID ? (
        <div className="my-3">
          <IntermediateView />
        </div>
      ) : (
        <pre> synthesis is not selected </pre>
      )}
    </>
  );
};
