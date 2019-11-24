import * as React from "react";
import { Button } from "react-bootstrap";
import { haskellApiService as api } from "../../services/HaskellApiService";
import { AppContext, IAppContext, SelectedNodeId } from "./AppContext";
import { AxiosPromise, AxiosResponse, AxiosError } from "axios";

import "./AppNavbar.scss";

export const AppNavbarButtons: React.FC = () => {
  const appContext = React.useContext(AppContext) as IAppContext;

  function requestNidBy<T extends Array<any>>(requestJob: (...args: T) => AxiosPromise, ...args: T) {
    return () => {
      requestJob(...args)
        .then((response: AxiosResponse<SelectedNodeId>) => {
          appContext.selectNode(response.data);
        })
        .catch((err: AxiosError) => alert(err));
    };
  }

  const btnAttrs = {
    className: "mr-1 btn-sm btn-secondary appNavbarButton",
  };

  return (
    <div className="d-flex">
      <div className="mr-3">
        <Button {...btnAttrs} onClick={requestNidBy(api.stateOfTheArtSynthesis, appContext.selectedNodeId)}>
          state-of-the-art
        </Button>
        <Button {...btnAttrs} onClick={requestNidBy(api.simpleSynthesis, appContext.selectedNodeId)}>
          simple
        </Button>
        <Button {...btnAttrs} onClick={requestNidBy(api.smartBindSynthesisIO, appContext.selectedNodeId)}>
          with-smart-bind
        </Button>
        <Button {...btnAttrs} onClick={requestNidBy(api.allBindsAndRefsIO, appContext.selectedNodeId)}>
          bind-and-refactors
        </Button>
      </div>
      <div>
        <Button {...btnAttrs} onClick={requestNidBy(api.allBestThread, appContext.selectedNodeId, 2)}>
          ∀-best-thread-2
        </Button>
        <Button {...btnAttrs} onClick={requestNidBy(api.allBestThread, appContext.selectedNodeId, 1)}>
          ∀-best-thread-1
        </Button>
        <Button {...btnAttrs} onClick={requestNidBy(api.allBestThread, appContext.selectedNodeId, 0)}>
          best-thread
        </Button>
        <Button {...btnAttrs} onClick={requestNidBy(api.obviousBindThread, appContext.selectedNodeId)}>
          obvious-bind-thread
        </Button>
      </div>
    </div>
  );
};
