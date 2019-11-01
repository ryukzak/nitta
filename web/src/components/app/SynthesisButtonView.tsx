import * as React from "react";
import { Button } from "react-bootstrap";
import { haskellApiService } from "../../services/HaskellApiService";
import { AppContext, IAppContext, SelectedNodeId } from "./AppContext";
import { AxiosResponse, AxiosError } from "axios";

import "./AppNavbar.scss";

export const SynthesisButtonView: React.FC = () => {
  const appContext = React.useContext(AppContext) as IAppContext;

  function simpleSynthesis(nid: SelectedNodeId) {
    haskellApiService
      .simpleSynthesis(nid)
      .then((response: AxiosResponse<SelectedNodeId>) => {
        appContext.selectNode(response.data);
      })
      .catch((err: AxiosError) => alert(err));
  }

  function smartBindSynthesisIO(nid: SelectedNodeId) {
    haskellApiService
      .smartBindSynthesisIO(nid)
      .then((response: AxiosResponse<SelectedNodeId>) => {
        appContext.selectNode(response.data);
      })
      .catch((err: AxiosError) => alert(err));
  }

  function allBestThread(nid: SelectedNodeId, n: number) {
    haskellApiService
      .allBestThread(nid, n)
      .then((response: AxiosResponse<SelectedNodeId>) => {
        appContext.selectNode(response.data);
      })
      .catch((err: AxiosError) => alert(err));
  }

  function obviousBindThread(nid: SelectedNodeId) {
    haskellApiService
      .obviousBindThread(nid)
      .then((response: AxiosResponse<SelectedNodeId>) => {
        appContext.selectNode(response.data);
      })
      .catch((err: AxiosError) => alert(err));
  }

  const buttonAttrs = {
    className: "mr-1 mb-1 btn-sm btn-secondary p-1.5",
    bsPrefix: "syn",
  };

  return (
    <div className="d-flex">
      <div className="mr-3">
        <Button {...buttonAttrs} onClick={() => simpleSynthesis(appContext.selectedNodeId)}>
          Simple synthesis
        </Button>
        <Button {...buttonAttrs} onClick={() => smartBindSynthesisIO(appContext.selectedNodeId)}>
          Smart bind synthesis
        </Button>
      </div>
      <div>
        <Button {...buttonAttrs} onClick={() => allBestThread(appContext.selectedNodeId, 2)}>
          All best tread 2
        </Button>
        <Button {...buttonAttrs} onClick={() => allBestThread(appContext.selectedNodeId, 1)}>
          All best tread 1
        </Button>
        <Button {...buttonAttrs} onClick={() => allBestThread(appContext.selectedNodeId, 0)}>
          Best tread
        </Button>
        <Button {...buttonAttrs} onClick={() => obviousBindThread(appContext.selectedNodeId)}>
          Obvious bind thread
        </Button>
      </div>
    </div>
  );
};
