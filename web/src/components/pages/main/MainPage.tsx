import * as React from "react";
import SynthesisButtonView from "./SynthesisButtonView";
import SynthesisGraphView from "./SynthesisGraphView";
import { AppContext, IAppContext } from "../../app/AppContext";
import { useContext } from "react";

export default function MainPage() {
  const appContext = useContext(AppContext) as IAppContext;

  return (
    <div className="d-flex flex-column">
      <SynthesisGraphView
        selectedNid={appContext.selectedNodeId}
        selectNode={appContext.selectNode}
        refreshGraph={appContext.reloadSelectedNode}
      />
      <SynthesisButtonView selectedNodeId={appContext.selectedNodeId} selectNode={appContext.selectNode} />
    </div>
  );
}
