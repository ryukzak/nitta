import * as React from "react";
import { Button } from "react-bootstrap";
import { SynthesisGraphView } from "./SynthesisGraphView";
import { AppContext, IAppContext } from "../../app/AppContext";

export const SynthesisGraphContainer: React.FC = () => {
  const appContext = React.useContext(AppContext) as IAppContext;

  const step = 100;
  const minHeight = 200;
  const [height, setHeight] = React.useState<number>(minHeight);

  const buttonAttrs = {
    className: "btn btn-sm mr-3",
    variant: "link" as any,
  };

  const expandSynthesisGraphView = () => setHeight(height + step);

  const reduceSynthesisGraphView = () => height > minHeight ? setHeight(height - step) : null ;

  return (
    <div className="flex-grow-1">
      <div className="d-flex justify-content-between m-2">
        <div className="mr-3">
          <Button {...buttonAttrs} onClick={() => expandSynthesisGraphView()}>
            Expand
          </Button>
          <Button {...buttonAttrs} onClick={() => reduceSynthesisGraphView()}>
            Reduce
          </Button>
          <Button {...buttonAttrs} onClick={() => appContext.reloadSelectedNode()}>
            Refresh
          </Button>
        </div>
        <span className="text-muted">black - processed node; white - in progress node; green - succees synthesis</span>
      </div>
      <div className="justify-content-center bg-light border" style={{ height: height }}>
        <SynthesisGraphView />
      </div>
    </div>
  );
};
