import * as React from "react";
import { Button } from "react-bootstrap";
import SynthesisGraph from "./SynthesisGraph";
import { SelectedNodeId } from "../../app/AppContext";

// TODO: REWRITE/REFACTOR COMPONENT "SynthesisGraphView"

export interface ISynthesisGraphViewProps {
  selectedNid: SelectedNodeId;
  selectNode: (nid: SelectedNodeId) => void;
  refreshGraph: () => void;
}

export interface ISynthesisGraphViewState {
  selectedNid: SelectedNodeId;
  selectNode: (nid: SelectedNodeId) => void;
  refreshGraph: () => void;
  minSynthesisGraphHeight: number;
  synthesisGraphHeight: number;
}

export default class SynthesisGraphView extends React.Component<ISynthesisGraphViewProps, ISynthesisGraphViewState> {
  private containerRef = React.createRef<HTMLDivElement>();
  private buttonRef = React.createRef<HTMLDivElement>();

  constructor(props: ISynthesisGraphViewProps) {
    super(props);
    this.state = {
      selectedNid: props.selectedNid,
      selectNode: props.selectNode,
      refreshGraph: props.refreshGraph,
      minSynthesisGraphHeight: 200,
      synthesisGraphHeight: 0,
    };
  }

  componentWillReceiveProps(newProps: ISynthesisGraphViewProps) {
    this.setState({
      selectedNid: newProps.selectedNid,
      selectNode: newProps.selectNode,
    });
  }

  componentDidMount = () => {
    if (this.containerRef.current != null && this.buttonRef.current != null) {
      this.setState({
        synthesisGraphHeight: this.containerRef.current.clientHeight - this.buttonRef.current.clientHeight,
      });
    }
  };

  resizeSynthesisGraphView = (expand: boolean) => {
    var currentHeight: number = this.state.synthesisGraphHeight;
    if (expand) {
      this.setState({
        synthesisGraphHeight: currentHeight += 100,
      });
    } else if (currentHeight > this.state.minSynthesisGraphHeight) {
      this.setState({
        synthesisGraphHeight: currentHeight -= 100,
      });
    }
  };

  public render() {
    return (
      <div className="flex-grow-1" ref={this.containerRef}>
        <div className="d-flex justify-content-between my-1 mx-2" ref={this.buttonRef}>
          <div className="d-flex flex-row my-auto">
            <Button
              className="btn btn-link btn-sm bg-transparent mr-3"
              style={{ border: 0 }}
              onClick={() => this.resizeSynthesisGraphView(true)}
            >
              Expand
            </Button>
            <Button
              className="btn btn-link btn-sm bg-transparent mr-3"
              style={{ border: 0 }}
              onClick={() => this.resizeSynthesisGraphView(false)}
            >
              Reduce
            </Button>
            <Button
              className="btn btn-link btn-sm bg-transparent mr-3"
              style={{ border: 0 }}
              onClick={() => this.state.refreshGraph()}
            >
              Refresh
            </Button>
          </div>
          <span className="text-muted">
            black - processed node; white - in progress node; green - succees synthesis
          </span>
        </div>
        <div
          className="justify-content-center bg-light border"
          style={{ height: this.state.synthesisGraphHeight, minHeight: this.state.minSynthesisGraphHeight }}
        >
          <SynthesisGraph selectedNId={this.state.selectedNid} onNIdChange={this.state.selectNode} />
        </div>
      </div>
    );
  }
}
