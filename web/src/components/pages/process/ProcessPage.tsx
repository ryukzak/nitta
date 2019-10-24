import * as React from "react";
import { ProcessView } from "./ProcessView";
import { SynthesisHistoryView } from "./SynthesisHistoryView";
import { EndpointOptions } from "./EndpointOptios";
import { SelectedNodeId } from "../../app/AppContext";

export interface IProcessPageProps {
  nId: SelectedNodeId;
}

export interface IProcessPageState {
  nId: SelectedNodeId;
}

export default class ProcessPage extends React.Component<IProcessPageProps, IProcessPageState> {
  constructor(props: IProcessPageProps) {
    super(props);

    this.state = {
      nId: props.nId,
    };
  }

  componentWillReceiveProps(props: IProcessPageProps) {
    if (this.state.nId !== props.nId) {
      this.setState({ nId: props.nId });
    }
  }

  render() {
    return (
      <div>
        {this.state.nId != null && (
          <div className="d-flex flex-row m-2">
            <div className="col-md-6">
              <ProcessView nId={this.state.nId} />
            </div>
            <div className="col-md-6">
              Synthesis history :
              <SynthesisHistoryView nId={this.state.nId} reverse={false} />
              <br />
              EndpointOptions :
              <EndpointOptions nId={this.state.nId} />
            </div>
          </div>
        )}
      </div>
    );
  }
}
