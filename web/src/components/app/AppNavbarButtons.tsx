import * as React from "react";
import { DropdownButton, Dropdown } from "react-bootstrap";
import { haskellApiService as api } from "../../services/HaskellApiService";
import { AppContext, IAppContext } from "./AppContext";
import { requestNidBy } from "../../utils/componentUtils";

import "./AppNavbar.scss";

export const AppNavbarButtons: React.FC = () => {
  const ctx = React.useContext(AppContext) as IAppContext;

  const btnAttrs = {
    className: "mr-1 btn-sm btn-secondary appNavbarButton"
  };

  return (
    <div className="d-flex">
      <div className="mr-3">
        <DropdownButton id="dropdown-basic-button" title="Synthesis methods" size="sm">
          <Dropdown.Item {...btnAttrs} onClick={requestNidBy(ctx, api.stateOfTheArtSynthesis, ctx.selectedNodeId)}>
            state-of-the-art
          </Dropdown.Item>
          <Dropdown.Item {...btnAttrs} onClick={requestNidBy(ctx, api.simpleSynthesis, ctx.selectedNodeId)}>
            simple
          </Dropdown.Item>
          <Dropdown.Item {...btnAttrs} onClick={requestNidBy(ctx, api.smartBindSynthesisIO, ctx.selectedNodeId)}>
            with-smart-bind
          </Dropdown.Item>
          <Dropdown.Item {...btnAttrs} onClick={requestNidBy(ctx, api.allBindsAndRefsIO, ctx.selectedNodeId)}>
            bind-and-refactors
          </Dropdown.Item>
        </DropdownButton>
      </div>
      <div className="mr-3">
        <DropdownButton id="dropdown-basic-button" title="Synthesis step" size="sm">
          <Dropdown.Item {...btnAttrs} onClick={requestNidBy(ctx, api.allBestThread, ctx.selectedNodeId, 2)}>
            ∀-best-thread-2
          </Dropdown.Item>
          <Dropdown.Item {...btnAttrs} onClick={requestNidBy(ctx, api.allBestThread, ctx.selectedNodeId, 1)}>
            ∀-best-thread-1
          </Dropdown.Item>
          <Dropdown.Item {...btnAttrs} onClick={requestNidBy(ctx, api.allBestThread, ctx.selectedNodeId, 0)}>
            best-thread
          </Dropdown.Item>
          <Dropdown.Item {...btnAttrs} onClick={requestNidBy(ctx, api.obviousBindThread, ctx.selectedNodeId)}>
            obvious-bind-thread
          </Dropdown.Item>
        </DropdownButton>
      </div>
    </div>
  );
};
