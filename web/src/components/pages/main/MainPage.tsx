import * as React from "react";
import NodeView from "./NodeView";
import SynthesisGraphView from "./SynthesisGraphView";

export default function MainPage() {
  return (
    <div className="h-100 d-flex flex-column">
      <SynthesisGraphView/>
      <NodeView/>
    </div>
  );
}
