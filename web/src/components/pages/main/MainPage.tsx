import * as React from "react";
import SynthesisButtonView from "./SynthesisButtonView";
import SynthesisGraphView from "./SynthesisGraphView";
import { AppContextConsumer } from "../../app/AppContext";
import NodeView from "../node/NodeView";

export default function MainPage() {
  
  return (
    <AppContextConsumer>
      {appContext => appContext && (
        <div className="d-flex flex-column" >
            <SynthesisGraphView selectedNid={appContext.selectedNodeId} selectNode={appContext.selectNode} refreshGraph={appContext.reloadSelectedNode} />
            <SynthesisButtonView selectedNodeId={appContext.selectedNodeId} selectNode={appContext.selectNode} />
            <NodeView selectedNId={appContext.selectedNodeId}/>
        </div>
      )}
    </AppContextConsumer>
  );
}
