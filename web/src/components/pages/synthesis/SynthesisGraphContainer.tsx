import * as React from "react";
import { Button } from "react-bootstrap";
import { SynthesisGraphView } from "./SynthesisGraphView";
import { AppContext, IAppContext } from "../../app/AppContext";

export const SynthesisGraphContainer: React.FC = () => {
  const appContext = React.useContext(AppContext) as IAppContext;

  const minSynthesisGraphHeight = 200;
  const [synthesisGraphHeight, setSynthesisGraphHeight] = React.useState<number>(minSynthesisGraphHeight);

  const buttonAttrs = {
    className: "btn btn-sm mr-3"
  };

  function resizeSynthesisGraphView(expand: boolean) {
    if (expand) {
      setSynthesisGraphHeight(synthesisGraphHeight + 100);
    } else if (synthesisGraphHeight > minSynthesisGraphHeight) {
      setSynthesisGraphHeight(synthesisGraphHeight - 100);
    }
  }

  return (
    <div className="flex-grow-1">
      <div className="d-flex justify-content-between m-2">
        <div className="mr-3">
          <Button {...buttonAttrs} variant="link" onClick={() => resizeSynthesisGraphView(true)}>
            Expand
          </Button>
          <Button {...buttonAttrs} variant="link" onClick={() => resizeSynthesisGraphView(false)}>
            Reduce
          </Button>
          <Button {...buttonAttrs} variant="link" onClick={() => appContext.reloadSelectedNode()}>
            Refresh
          </Button>
        </div>
        <span className="text-muted">black - processed node; white - in progress node; green - succees synthesis</span>
      </div>
      <div className="justify-content-center bg-light border" style={{ height: synthesisGraphHeight }}>
        <SynthesisGraphView />
      </div>
    </div>
  );
};
