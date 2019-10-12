import * as React from "react";
import SynthesisGraph from "./SynthesisGraphView";
import ButtonView from "./BottomButtonView";
import TopButtonView from "./TopButtonView";

export default function MainPage() {
  return (
    <div className="h-100 d-flex flex-column">
      <TopButtonView/>
      <SynthesisGraph/>
      <ButtonView/>
    </div>
  );
}
