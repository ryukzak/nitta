import { AppContext, type IAppContext } from "app/AppContext";
import { type FC, useContext, useState } from "react";
import { Button, OverlayTrigger, Popover } from "react-bootstrap";
import * as Icon from "react-bootstrap-icons";
import {
  api,
  reLastSid,
  sidSeparator,
  synthesize,
} from "services/HaskellApiService";
import { SynthesisGraphRender } from "./SynthesisGraph/Render";

export const SynthesisGraph: FC = () => {
  const appContext = useContext(AppContext) as IAppContext;

  const step = 100;
  const minHeight = 200;
  const [height, setHeight] = useState<number>(minHeight);

  const buttonAttrs = {
    className: "btn btn-sm mr-3",
    variant: "link" as any,
  };

  const expandSynthesisGraphView = () => setHeight(height + step);

  const reduceSynthesisGraphView = () =>
    height > minHeight ? setHeight(height - step) : null;

  const backNavigation = () => {
    const newId = appContext.selectedSid.replace(reLastSid, "");
    if (newId != null && newId.length !== 0) appContext.setSid(newId);
    else appContext.setSid(sidSeparator);
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
          <Button {...buttonAttrs} onClick={() => backNavigation()}>
            Back
          </Button>
          <Button
            {...buttonAttrs}
            onClick={synthesize(
              appContext,
              api.bestStep,
              appContext.selectedSid,
            )}
          >
            Forward
          </Button>
        </div>
        <span className="text-muted">
          <OverlayTrigger
            trigger="click"
            placement="left"
            overlay={
              <Popover id={`popover-positioned-left`}>
                <Popover.Header>Legend</Popover.Header>
                <Popover.Body>
                  <p>black - processed node (subforest was evaluated)</p>
                  <p>
                    white - not precessed node (subforest was not evaluated)
                  </p>
                  <p>green - succees synthesis</p>
                  <p>blue - current sected node</p>
                </Popover.Body>
              </Popover>
            }
          >
            <Icon.InfoCircle />
          </OverlayTrigger>
        </span>
      </div>
      <div
        className="justify-content-center bg-light border"
        style={{ height: height }}
      >
        <SynthesisGraphRender />
      </div>
    </div>
  );
};
