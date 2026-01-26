import { AppContext, type IAppContext } from "app/AppContext";
import { IntermediateView } from "components/IntermediateView";
import { MicroarchitectureView } from "components/MicroarchitectureView";
import { TreeInfoView } from "components/TreeInfoView";
import React, { type FC, useContext } from "react";

export type INodeScreenProps = {};

export const NodeScreen: FC<INodeScreenProps> = (props) => {
  const { selectedSid } = useContext(AppContext) as IAppContext;

  if (!selectedSid) return <pre> synthesis is not selected </pre>;

  return (
    <div className="m-3">
      <pre>{selectedSid}</pre>

      <h3>Tree info:</h3>
      <TreeInfoView />

      <h3 className="mt-3">Dataflow graph:</h3>
      <IntermediateView />

      <h3 className="mt-3">Microarchitecture and transfers:</h3>
      <MicroarchitectureView />
    </div>
  );
};
