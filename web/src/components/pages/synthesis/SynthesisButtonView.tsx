import * as React from "react";
import { Button } from "react-bootstrap";
import { haskellApiService } from "../../../services/HaskellApiService";
import { SelectedNodeId } from "../../app/AppContext";

// TODO: REWRITE/REFACTOR COMPONENT "SynthesisButtonView"

export interface ISynthesisButtonViewProps {
  selectedNodeId: SelectedNodeId;
  selectNode: (nid: SelectedNodeId) => void;
}

export interface ISynthesisButtonViewState {
  selectedNodeId: SelectedNodeId;
  selectNode: (nid: SelectedNodeId) => void;
}

export default class SynthesisButtonView extends React.Component<ISynthesisButtonViewProps, ISynthesisButtonViewState> {
  constructor(props: ISynthesisButtonViewProps) {
    super(props);
    this.state = {
      selectedNodeId: props.selectedNodeId,
      selectNode: props.selectNode,
    };
  }

  componentWillReceiveProps(newProps: ISynthesisButtonViewProps) {
    this.setState({
      selectedNodeId: newProps.selectedNodeId,
      selectNode: newProps.selectNode,
    });
  }

  simpleSynthesis(nid: any, deep?: any) {
    if (nid === undefined || nid === null) return;
    console.debug("NodeView:simpleSynthesis(", nid, " )");
    haskellApiService
      .simpleSynthesis(nid)
      .then((response: any) => {
        let newNid = response.data;
        this.setState({ selectedNodeId: newNid });
        this.state.selectNode(newNid);
      })
      .catch((err: any) => alert(err));
  }

  smartBindSynthesisIO(nid: any, deep?: any) {
    if (nid === undefined || nid === null) return;
    console.debug("NodeView:simpleSynthesis(", nid, " )");
    haskellApiService
      .smartBindSynthesisIO(nid)
      .then((response: any) => {
        let newNid = response.data;
        this.setState({ selectedNodeId: newNid });
        this.state.selectNode(newNid);
      })
      .catch((err: any) => alert(err));
  }

  allBestThread(nid: any, n: any) {
    if (nid === undefined || nid === null) return;
    console.debug("NodeView:allBestThread(", nid, n, " )");
    haskellApiService
      .allBestThread(nid, n)
      .then((response: any) => {
        let newNid = response.data;
        this.setState({ selectedNodeId: newNid });
        this.state.selectNode(newNid);
      })
      .catch((err: any) => alert(err));
  }

  obviousBindThread(nid: any) {
    if (nid === undefined || nid === null) return;
    console.debug("NodeView:obviousBindThread(", nid, " )");
    haskellApiService
      .obviousBindThread(nid)
      .then((response: any) => {
        let newNid = response.data;
        this.setState({ selectedNodeId: newNid });
        this.state.selectNode(newNid);
      })
      .catch((err: any) => alert(err));
  }

  public render() {
    const buttonAttrs = {
      className: "mr-2 btn-sm btn-secondary",
    };

    return (
      <div className="d-flex">
        <div className="mr-3">
          <Button {...buttonAttrs} onClick={() => this.simpleSynthesis(this.state.selectedNodeId)}>
            Simple synthesis
          </Button>
          <Button {...buttonAttrs} onClick={() => this.smartBindSynthesisIO(this.state.selectedNodeId)}>
            Smart bind synthesis
          </Button>
        </div>
        <div>
          <Button {...buttonAttrs} onClick={() => this.allBestThread(this.state.selectedNodeId, 2)}>
            All best tread 2
          </Button>
          <Button {...buttonAttrs} onClick={() => this.allBestThread(this.state.selectedNodeId, 1)}>
            All best tread 1
          </Button>
          <Button {...buttonAttrs} onClick={() => this.allBestThread(this.state.selectedNodeId, 0)}>
            Best tread
          </Button>
          <Button {...buttonAttrs} onClick={() => this.obviousBindThread(this.state.selectedNodeId)}>
            Obvious bind thread
          </Button>
        </div>
      </div>
    );
  }
}
