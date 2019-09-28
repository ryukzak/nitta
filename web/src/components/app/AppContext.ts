import React from "react";

export type SelectedNodeId = number | null;

export interface IAppContext {
  selectedNodeId: SelectedNodeId;
  selectNode: (id: SelectedNodeId) => void;
  reloadSelectedNode: () => void;
}

function notImplementedFunction() {
  throw Error("A function is used outside of context that implements it.");
}

export default React.createContext<IAppContext>({
  selectedNodeId: null,
  selectNode: notImplementedFunction,
  reloadSelectedNode: notImplementedFunction,
});
