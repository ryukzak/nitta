import * as React from "react";
import { Button } from "react-bootstrap";
import { haskellApiService as api } from "../../services/HaskellApiService";
import { AppContext, IAppContext } from "./AppContext";
import { requestNidBy } from "../../utils/componentUtils";

import "./AppNavbar.scss";

export const AppNavbarButtons: React.FC = () => {
  const appContext = React.useContext(AppContext) as IAppContext;

  const btnAttrs = {
    className: "mr-1 btn-sm btn-secondary appNavbarButton"
  };

  return (
    <div className="d-flex">
      <div className="mr-3">
        <Button {...btnAttrs} onClick={requestNidBy(appContext, api.stateOfTheArtSynthesis, appContext.selectedNodeId)}>
          state-of-the-art
        </Button>
        <Button {...btnAttrs} onClick={requestNidBy(appContext, api.simpleSynthesis, appContext.selectedNodeId)}>
          simple
        </Button>
        <Button {...btnAttrs} onClick={requestNidBy(appContext, api.smartBindSynthesisIO, appContext.selectedNodeId)}>
          with-smart-bind
        </Button>
        <Button {...btnAttrs} onClick={requestNidBy(appContext, api.allBindsAndRefsIO, appContext.selectedNodeId)}>
          bind-and-refactors
        </Button>
      </div>
      <div>
        <Button {...btnAttrs} onClick={requestNidBy(appContext, api.allBestThread, appContext.selectedNodeId, 2)}>
          ∀-best-thread-2
        </Button>
        <Button {...btnAttrs} onClick={requestNidBy(appContext, api.allBestThread, appContext.selectedNodeId, 1)}>
          ∀-best-thread-1
        </Button>
        <Button {...btnAttrs} onClick={requestNidBy(appContext, api.allBestThread, appContext.selectedNodeId, 0)}>
          best-thread
        </Button>
        <Button {...btnAttrs} onClick={requestNidBy(appContext, api.obviousBindThread, appContext.selectedNodeId)}>
          obvious-bind-thread
        </Button>
      </div>
    </div>
  );
};
