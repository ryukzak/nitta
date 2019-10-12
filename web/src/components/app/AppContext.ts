import React from "react";

export type SelectedNodeId = number | null;

export interface IAppContext {
  selectedNodeId: SelectedNodeId;
  selectNode: (id: SelectedNodeId) => void;
  reloadSelectedNode: () => void;
  changedSynthesisGraphHeight: (newSynthesisGraphHeight: number) => void;
  synthesisGraphHeight: number;
  minSynthesisGraphHeight: number;
}

function notImplementedFunction() {
  throw Error("A function is used outside of context that implements it.");
}

export default React.createContext<IAppContext>({
  selectedNodeId: null,
  selectNode: notImplementedFunction,
  reloadSelectedNode: notImplementedFunction,
  changedSynthesisGraphHeight: notImplementedFunction,
  synthesisGraphHeight: 0,
  minSynthesisGraphHeight: 0
});
