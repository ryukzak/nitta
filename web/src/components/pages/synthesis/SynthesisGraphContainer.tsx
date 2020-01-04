import * as React from "react";
import { Button } from "react-bootstrap";
import { SynthesisGraphView } from "./SynthesisGraphView";
import { AppContext, IAppContext, reLastNidStep, SelectedNodeId } from "../../app/AppContext";
import { haskellApiService } from "../../../services/HaskellApiService";
import { AxiosResponse, AxiosError } from "axios";

export const SynthesisGraphContainer: React.FC = () => {
  const appContext = React.useContext(AppContext) as IAppContext;

  const step = 100;
  const minHeight = 200;
  const [height, setHeight] = React.useState<number>(minHeight);

  const nextNidStep = new RegExp(appContext.selectedNodeId + "-[^-]*");
  const firstNidStep = /-[^-]*/;

  const buttonAttrs = {
    className: "btn btn-sm mr-3",
    variant: "link" as any
  };

  const expandSynthesisGraphView = () => setHeight(height + step);

  const reduceSynthesisGraphView = () => (height > minHeight ? setHeight(height - step) : null);

  const backNavigation = () => {
    let newId = appContext.selectedNodeId.replace(reLastNidStep, "");
    if (newId != null && newId.length !== 0) appContext.selectNode(newId);
    else appContext.selectNode("-");
  };

  const forwardNavigation = () => {
    haskellApiService
      .allBestThread(appContext.selectedNodeId, 0)
      .then((response: AxiosResponse<SelectedNodeId>) => {
        let newId = response.data;
        if (appContext.selectedNodeId === "-") newId = firstNidStep.exec(newId)![0];
        else newId = nextNidStep.exec(newId)![0];

        if (newId != null) appContext.selectNode(newId);
      })
      .catch((err: AxiosError) => console.log(err));
  };

  return (
    <div className="flex-grow-1">
      <div className="d-flex justify-content-between m-2">
        <div>
          <Button {...buttonAttrs} onClick={() => expandSynthesisGraphView()}>
            Expand
          </Button>
          <Button {...buttonAttrs} onClick={() => reduceSynthesisGraphView()}>
            Reduce
          </Button>
          <Button {...buttonAttrs} onClick={() => appContext.reloadSelectedNode()}>
            Refresh
          </Button>
          <Button {...buttonAttrs} onClick={() => backNavigation()}>
            Back
          </Button>
          <Button {...buttonAttrs} onClick={() => forwardNavigation()}>
            Forward
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
