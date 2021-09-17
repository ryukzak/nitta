import Axios from "axios";
import React, { useContext, FC } from "react";
import { DropdownButton, Dropdown } from "react-bootstrap";
import { synthesize, api } from "services/HaskellApiService";
import { AppContext, IAppContext } from "./AppContext";
import useRequestCancellation from "hooks/useApiRequestCancellation";

import "./AppNavbar.scss";

export const AppNavbarButtons: FC = () => {
  const ctx = useContext(AppContext) as IAppContext;

  const btnAttrs = {
    className: "mr-1 btn-sm btn-secondary appNavbarButton",
  };

  const source = Axios.CancelToken.source();
  useRequestCancellation(source);

  return (
    <div className="d-flex">
      <div className="mr-3">
        <DropdownButton id="dropdown-basic-button" title="Synthesis methods" size="sm">
          <Dropdown.Item
            {...btnAttrs}
            onClick={synthesize(ctx, api.stateOfTheArtSynthesis, ctx.selectedSID, source.token)}
          >
            state-of-the-art
          </Dropdown.Item>
          <Dropdown.Item {...btnAttrs} onClick={synthesize(ctx, api.simpleSynthesis, ctx.selectedSID, source.token)}>
            simple
          </Dropdown.Item>
          <Dropdown.Item
            {...btnAttrs}
            onClick={synthesize(ctx, api.smartBindSynthesisIO, ctx.selectedSID, source.token)}
          >
            with-smart-bind
          </Dropdown.Item>
          <Dropdown.Item {...btnAttrs} onClick={synthesize(ctx, api.allBindsAndRefsIO, ctx.selectedSID, source.token)}>
            bind-and-refactors
          </Dropdown.Item>
        </DropdownButton>
      </div>
      <div className="mr-3">
        <DropdownButton id="dropdown-basic-button" title="Synthesis step" size="sm">
          <Dropdown.Item {...btnAttrs} onClick={synthesize(ctx, api.allBestThread, ctx.selectedSID, 2, source.token)}>
            ∀-best-thread-2
          </Dropdown.Item>
          <Dropdown.Item {...btnAttrs} onClick={synthesize(ctx, api.allBestThread, ctx.selectedSID, 1, source.token)}>
            ∀-best-thread-1
          </Dropdown.Item>
          <Dropdown.Item {...btnAttrs} onClick={synthesize(ctx, api.allBestThread, ctx.selectedSID, 0, source.token)}>
            best-thread
          </Dropdown.Item>
          <Dropdown.Item {...btnAttrs} onClick={synthesize(ctx, api.obviousBindThread, ctx.selectedSID, source.token)}>
            obvious-bind-thread
          </Dropdown.Item>
        </DropdownButton>
      </div>
    </div>
  );
};
