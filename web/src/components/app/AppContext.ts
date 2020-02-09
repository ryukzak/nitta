import React from "react";

export const nInSeparator = "-";
export const reLastNidStep = /-[^-]*$/;

export type NodeId = string;

export interface IAppContext {
  selectedNodeId: NodeId;
  selectNode: (id: NodeId) => void;
  reloadSelectedNode: () => void;
}

// exported to use with useContext hook in functional components
export const AppContext = React.createContext<IAppContext | null>(null);

export const AppContextProvider = AppContext.Provider;
export const AppContextConsumer = AppContext.Consumer;
