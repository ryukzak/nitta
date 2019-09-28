import * as React from "react";

export interface INodeViewProps {}

export interface INodeViewState {}

export default class NodeView extends React.Component<INodeViewProps, INodeViewState> {
  constructor(props: INodeViewProps) {
    super(props);

    this.state = {};
  }

  public render() {
    return (
      <div className="text-center p-5 bg-light border m-2 flex-grow-1">
        <h1 className="text-black-50">Node View</h1>
      </div>
    );
  }
}
