import * as React from "react";
import NodeView from "./NodeView";
import { AppContextConsumer } from "../../app/AppContext";

export default function NodePage() {

  return (
    <AppContextConsumer>
      {appContext => appContext && (
        <div className="h-100 d-flex flex-column">
          <NodeView selectedNId={appContext.selectedNodeId} />
        </div>
      )}
    </AppContextConsumer>
  );
}
