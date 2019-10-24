import * as React from "react";
import "react-table/react-table.css";
import { haskellApiService } from "../../../services/HaskellApiService";
import { IntermediateView } from "./IntermadiateView";
import { JsonView } from "./JsonView";

export interface INodeViewProps {
  selectedNId: any;
  synthesisStatus?: any;
}

export interface INodeViewState {
  synthesisNode: any;
  selectedNId: any;
  synthesisStatus: any;
  view: string;
  model: any;
  endpointOptions: any[] | null;
  scOptions: any;
}

export default class NodeView extends React.Component<INodeViewProps, INodeViewState> {
  constructor(props: INodeViewProps) {
    super(props);
    this.state = {
      synthesisNode: null,
      selectedNId: props.selectedNId,
      synthesisStatus: props.synthesisStatus,
      view: "update",
      model: null,
      endpointOptions: null,
      scOptions: null,
    };
    // FIXME:
    this.handleViewChange(this.state.selectedNId, props.synthesisStatus, "synthesisNode");
  }

  handleViewChange(nid: any, synthesisStatus: any, view: string) {
    console.debug("NodeView:handleViewChange(", nid, ", ", view, " ) // this.state.view:", this.state.view);
    if (nid === undefined || nid === null) return;
    this.setState({ selectedNId: nid, synthesisStatus: synthesisStatus, view: "update" });
    this.updateModel(nid, "synthesisNode");
  }

  componentWillReceiveProps(props: INodeViewProps) {
    console.debug("NodeView:componentWillReceiveProps(", props, " )");
    let view = this.state.view;
    if (view === "update") view = "synthesisNode";
    if (this.state.selectedNId !== props.selectedNId) {
      this.handleViewChange(props.selectedNId, props.synthesisStatus, view);
    }
  }

  updateModel(nid: any, view: any) {
    haskellApiService
      .getNode(nid)
      .then((response: any) => {
        this.setState({
          synthesisNode: response.data,
          view: view,
        });
      })
      .catch((err: any) => console.log(err));
  }

  render() {
    return (
      <div>
        {this.state.selectedNId === null && <pre> synthesis is not selected </pre>}

        {this.state.selectedNId !== null && (
          <div>
            {this.state.view === "update" && <pre> updating... </pre>}
            {this.state.view === "synthesisNode" && (
              <div className="m-3">
                <div className="d-flex flex-row">
                  <div className="edgeGraphContainer">
                    <IntermediateView view={this.state.view} selectedNId={this.state.selectedNId} />
                  </div>
                  <div className="jsonViewContainer" style={{ verticalAlign: "top", width: "270px" }}>
                    <JsonView jsonData={this.state.synthesisNode.nModel.mUnit} label={"nModel.mUnit"} show={false} />
                  </div>
                  <div className="jsonViewContainer" style={{ display: "inline-block", verticalAlign: "top" }}>
                    <JsonView
                      jsonData={this.state.synthesisNode.nModel.mDataFlowGraph}
                      label={"nModel.mDataFlowGraph"}
                      show={false}
                    />
                  </div>
                  <div className="jsonViewContainer" style={{ display: "inline-block", verticalAlign: "top" }}>
                    <div>
                      <JsonView jsonData={this.state.synthesisNode.nId} label={"nId"} show={false} />
                    </div>
                    <br></br>
                    <div>
                      <JsonView jsonData={this.state.synthesisNode.nIsComplete} label={"nIsComplete"} show={false} />
                    </div>
                  </div>
                </div>
              </div>
            )}
          </div>
        )}
      </div>
    );
  }
}
