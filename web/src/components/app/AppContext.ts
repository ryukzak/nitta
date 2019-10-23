import React from "react";

export type SelectedNodeId = string | null;

export interface IAppContext {
  selectedNodeId: SelectedNodeId;
  selectNode: (id: SelectedNodeId) => void;
  reloadSelectedNode: () => void;
}

// exported to use with useContext hook in functional components
export const AppContext = React.createContext<IAppContext | null>(null);

export const AppContextProvider = AppContext.Provider;
export const AppContextConsumer = AppContext.Consumer;
