import React from "react";

export type SelectedNodeId = string | null;

export interface IAppContext {
  selectedNodeId: SelectedNodeId;
  selectNode: (id: SelectedNodeId) => void;
  reloadSelectedNode: () => void;
}

function notImplementedFunction() {
  throw Error("A function is used outside of context that implements it.");
}

const ctxt = React.createContext<IAppContext | null>(null);

export const AppContextProvider = ctxt.Provider;
  
export const AppContextConsumer = ctxt.Consumer;

// export default React.createContext<IAppContext>({
//   selectedNodeId: null,
//   selectNode: notImplementedFunction,
//   reloadSelectedNode: notImplementedFunction
// });
