import React, { useContext, useState, FC, useEffect } from "react";
import { Popover, OverlayTrigger, Button } from "react-bootstrap";
import * as Icon from "react-bootstrap-icons";

import { synthesize, api, reLastSID, sidSeparator } from "services/HaskellApiService";

import { SynthesisGraphRender } from "./SynthesisGraph/Render";
import { AppContext, IAppContext } from "app/AppContext";
import Axios from "axios";

export const SynthesisGraph: FC = () => {
  const appContext = useContext(AppContext) as IAppContext;

  const source = Axios.CancelToken.source();

  useEffect(() => {
    return () => {
      source.cancel();
    };
  }, [source]);

  const step = 100;
  const minHeight = 200;
  const [height, setHeight] = useState<number>(minHeight);

  const buttonAttrs = {
    className: "btn btn-sm mr-3",
    variant: "link" as any,
  };

  const expandSynthesisGraphView = () => setHeight(height + step);

  const reduceSynthesisGraphView = () => (height > minHeight ? setHeight(height - step) : null);

  const backNavigation = () => {
    let newId = appContext.selectedSID.replace(reLastSID, "");
    if (newId != null && newId.length !== 0) appContext.setSID(newId);
    else appContext.setSID(sidSeparator);
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
          <Button {...buttonAttrs} onClick={synthesize(appContext, api.bestStep, appContext.selectedSID, source.token)}>
            Forward
          </Button>
        </div>
        <span className="text-muted">
          <OverlayTrigger
            trigger="click"
            placement="left"
            overlay={
              <Popover id={`popover-positioned-left`}>
                <Popover.Title>Legend</Popover.Title>
                <Popover.Content>
                  <p>black - processed node (subforest was evaluated)</p>
                  <p>white - not precessed node (subforest was not evaluated)</p>
                  <p>green - succees synthesis</p>
                  <p>blue - current sected node</p>
                </Popover.Content>
              </Popover>
            }
          >
            <Icon.InfoCircle />
          </OverlayTrigger>
        </span>
      </div>
      <div className="justify-content-center bg-light border" style={{ height: height }}>
        <SynthesisGraphRender />
      </div>
    </div>
  );
};
