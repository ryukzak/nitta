import * as React from "react";
import { AppContextConsumer } from "../../app/AppContext";
import {EdgesView} from "./EdgesView"

export default function MainPage() {
  return (
    <AppContextConsumer>
      {appContext => appContext && (
        <div className="d-flex flex-grow-1 flex-column ">
          <EdgesView nid={appContext.selectedNodeId} onNidChange={appContext.selectNode} />
        </div>
      )}
    </AppContextConsumer>
  );
}
