import * as React from "react";
import { Button } from "react-bootstrap";
import { haskellApiService } from "../../services/HaskellApiService";
import { AppContext, IAppContext, SelectedNodeId } from "./AppContext";
import { AxiosPromise, AxiosResponse, AxiosError } from "axios";

import "./AppNavbar.scss";

export const SynthesisButtonView: React.FC = () => {
  const appContext = React.useContext(AppContext) as IAppContext;

  enum ButtonAct {
    SympleSynthesis,
    SmartBindSynthesisIO,
    AllBestThread,
    ObviousBindThread,
  }

  function buttonHandler(nid: SelectedNodeId, act: ButtonAct, n?: number) {
    let fun: () => AxiosPromise = () => {
      switch (act) {
        case ButtonAct.SympleSynthesis:
          return haskellApiService.simpleSynthesis(nid);
        case ButtonAct.SmartBindSynthesisIO:
          return haskellApiService.smartBindSynthesisIO(nid);
        case ButtonAct.AllBestThread:
          return haskellApiService.allBestThread(nid, n);
        case ButtonAct.ObviousBindThread:
          return haskellApiService.obviousBindThread(nid);
      }
    };

    fun()
      .then((response: AxiosResponse<SelectedNodeId>) => {
        appContext.selectNode(response.data);
      })
      .catch((err: AxiosError) => alert(err));
  }

  const buttonAttrs = {
    className: "mr-1 btn-sm btn-secondary appNavbarButton",
  };

  return (
    <div className="d-flex">
      <div className="mr-3">
        <Button {...buttonAttrs} onClick={() => buttonHandler(appContext.selectedNodeId, ButtonAct.SympleSynthesis)}>
          Simple synthesis
        </Button>
        <Button
          {...buttonAttrs}
          onClick={() => buttonHandler(appContext.selectedNodeId, ButtonAct.SmartBindSynthesisIO)}
        >
          Smart bind synthesis
        </Button>
      </div>
      <div>
        <Button {...buttonAttrs} onClick={() => buttonHandler(appContext.selectedNodeId, ButtonAct.AllBestThread, 2)}>
          All best tread 2
        </Button>
        <Button {...buttonAttrs} onClick={() => buttonHandler(appContext.selectedNodeId, ButtonAct.AllBestThread, 1)}>
          All best tread 1
        </Button>
        <Button {...buttonAttrs} onClick={() => buttonHandler(appContext.selectedNodeId, ButtonAct.AllBestThread, 0)}>
          Best tread
        </Button>
        <Button {...buttonAttrs} onClick={() => buttonHandler(appContext.selectedNodeId, ButtonAct.ObviousBindThread)}>
          Obvious bind thread
        </Button>
      </div>
    </div>
  );
};
