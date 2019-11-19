import * as React from "react";
import { Button } from "react-bootstrap";
import { haskellApiService } from "../../services/HaskellApiService";
import { AppContext, IAppContext, SelectedNodeId } from "./AppContext";
import { AxiosPromise, AxiosResponse, AxiosError } from "axios";

import "./AppNavbar.scss";

export const SynthesisButtonView: React.FC = () => {
  const appContext = React.useContext(AppContext) as IAppContext;

  function getRequestWrapper(requestJob: () => AxiosPromise) {
    return () => {
      requestJob()
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
        <Button
          {...buttonAttrs}
          onClick={getRequestWrapper(() => haskellApiService.simpleSynthesis(appContext.selectedNodeId))}
        >
          Simple synthesis
        </Button>
        <Button
          {...buttonAttrs}
          onClick={getRequestWrapper(() => haskellApiService.smartBindSynthesisIO(appContext.selectedNodeId))}
        >
          Smart bind synthesis
        </Button>
      </div>
      <div>
        <Button
          {...buttonAttrs}
          onClick={getRequestWrapper(() => haskellApiService.allBestThread(appContext.selectedNodeId, 2))}
        >
          All best tread 2
        </Button>
        <Button
          {...buttonAttrs}
          onClick={getRequestWrapper(() => haskellApiService.allBestThread(appContext.selectedNodeId, 1))}
        >
          All best tread 1
        </Button>
        <Button
          {...buttonAttrs}
          onClick={getRequestWrapper(() => haskellApiService.allBestThread(appContext.selectedNodeId, 0))}
        >
          Best tread
        </Button>
        <Button
          {...buttonAttrs}
          onClick={getRequestWrapper(() => haskellApiService.obviousBindThread(appContext.selectedNodeId))}
        >
          Obvious bind thread
        </Button>
      </div>
    </div>
  );
};
