import React from "react";

export type SelectedNodeId = string | null;

export interface IAppContext {
  selectedNodeId: SelectedNodeId;
  selectNode: (id: SelectedNodeId) => void;
  reloadSelectedNode: () => void;
}

const ctxt = React.createContext<IAppContext | null>(null);

export const AppContextProvider = ctxt.Provider;
export const AppContextConsumer = ctxt.Consumer;
