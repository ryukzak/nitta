import * as React from "react";
import SynthesisGraphView from "./SynthesisGraphView";
import { AppContext, IAppContext } from "../../app/AppContext";
import { useContext } from "react";

interface IMainPageProps {}

export const MainPage: React.FC<IMainPageProps> = props => {
  const appContext = useContext(AppContext) as IAppContext;

  return (
    <SynthesisGraphView
      selectedNid={appContext.selectedNodeId}
      selectNode={appContext.selectNode}
      refreshGraph={appContext.reloadSelectedNode}
    />
  );
};
