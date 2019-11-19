import * as React from "react";
import { Button } from "react-bootstrap";
import { haskellApiService } from "../../services/HaskellApiService";
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

  const buttonAttrs = {
    className: "mr-1 btn-sm btn-secondary appNavbarButton",
  };

  return (
    <div className="d-flex">
      <div className="mr-3">
        <Button {...buttonAttrs} onClick={requestNidBy(haskellApiService.simpleSynthesis, appContext.selectedNodeId)}>
          Simple synthesis
        </Button>
        <Button
          {...buttonAttrs}
          onClick={requestNidBy(haskellApiService.smartBindSynthesisIO, appContext.selectedNodeId)}
        >
          Smart bind synthesis
        </Button>
        <Button {...buttonAttrs} onClick={requestNidBy(haskellApiService.allBindsAndRefsIO, appContext.selectedNodeId)}>
          All bindings and refactorings
        </Button>
      </div>
      <div>
        <Button {...buttonAttrs} onClick={requestNidBy(haskellApiService.allBestThread, appContext.selectedNodeId, 2)}>
          All best tread 2
        </Button>
        <Button {...buttonAttrs} onClick={requestNidBy(haskellApiService.allBestThread, appContext.selectedNodeId, 1)}>
          All best tread 1
        </Button>
        <Button {...buttonAttrs} onClick={requestNidBy(haskellApiService.allBestThread, appContext.selectedNodeId, 0)}>
          Best tread
        </Button>
        <Button {...buttonAttrs} onClick={requestNidBy(haskellApiService.obviousBindThread, appContext.selectedNodeId)}>
          Obvious bind thread
        </Button>
      </div>
    </div>
  );
};
