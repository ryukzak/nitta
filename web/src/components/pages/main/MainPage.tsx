import * as React from "react";
import SynthesisGraph from "./SynthesisGraph";
import NodeView from "./NodeView";

export default function MainPage() {
  return (
    <div className="h-100 d-flex flex-column">
      <SynthesisGraph></SynthesisGraph>
      <NodeView></NodeView>
    </div>
  );
}
