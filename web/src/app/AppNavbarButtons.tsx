import React, { FC, useContext } from "react";
import { Dropdown, DropdownButton } from "react-bootstrap";
import { api, synthesize } from "services/HaskellApiService";
import { AppContext, IAppContext } from "./AppContext";

import "./AppNavbar.scss";

export const AppNavbarButtons: FC = () => {
  const ctx = useContext(AppContext) as IAppContext;

  const btnAttrs = {
    className: "mr-1 btn-sm btn-secondary appNavbarButton",
  };

  return (
    <div className="d-flex">
      <div className="mr-3">
        <DropdownButton id="dropdown-basic-button" title="Synthesis methods" size="sm">
          <Dropdown.Item {...btnAttrs} onClick={synthesize(ctx, api.stateOfTheArtSynthesis, ctx.selectedSid)}>
            state-of-the-art
          </Dropdown.Item>
          <Dropdown.Item {...btnAttrs} onClick={synthesize(ctx, api.simpleSynthesis, ctx.selectedSid)}>
            simple
          </Dropdown.Item>
          <Dropdown.Item {...btnAttrs} onClick={synthesize(ctx, api.smartBindSynthesisIO, ctx.selectedSid)}>
            with-smart-bind
          </Dropdown.Item>
          <Dropdown.Item {...btnAttrs} onClick={synthesize(ctx, api.allBindsAndRefsIO, ctx.selectedSid)}>
            bind-and-refactors
          </Dropdown.Item>
        </DropdownButton>
      </div>
      <div className="mr-3">
        <DropdownButton id="dropdown-basic-button" title="Synthesis step" size="sm">
          <Dropdown.Item {...btnAttrs} onClick={synthesize(ctx, api.allBestThread, ctx.selectedSid, 2)}>
            ∀-best-thread-2
          </Dropdown.Item>
          <Dropdown.Item {...btnAttrs} onClick={synthesize(ctx, api.allBestThread, ctx.selectedSid, 1)}>
            ∀-best-thread-1
          </Dropdown.Item>
          <Dropdown.Item {...btnAttrs} onClick={synthesize(ctx, api.allBestThread, ctx.selectedSid, 0)}>
            best-thread
          </Dropdown.Item>
          <Dropdown.Item {...btnAttrs} onClick={synthesize(ctx, api.obviousBindThread, ctx.selectedSid)}>
            obvious-bind-thread
          </Dropdown.Item>
        </DropdownButton>
      </div>
    </div>
  );
};
