import * as React from "react";
import { Button } from "react-bootstrap";
import { haskellApiService } from "../../../services/HaskellApiService";

export interface ISynthesisButtonViewProps {
  selectedNodeId: string | null;
  selectNode: (nid: string | null) => void;
}

export interface ISynthesisButtonViewState {
  selectedNodeId: string | null;
  selectNode: (nid: string | null) => void;
}

export default class SynthesisButtonView extends React.Component<ISynthesisButtonViewProps, ISynthesisButtonViewState> {
  constructor(props: ISynthesisButtonViewProps) {
    super(props);
    this.state = {
      selectedNodeId: props.selectedNodeId,
      selectNode: props.selectNode
    };
  }

  componentWillReceiveProps(newProps: ISynthesisButtonViewProps) {
    this.setState({
      selectedNodeId: newProps.selectedNodeId,
      selectNode: newProps.selectNode
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
    return (
      <div className="d-flex justify-content-between my-3  mx-2 pb-2">
        <div className="d-flex flex-row my-auto">
          <Button className="mr-3 p-1 btn-sm" onClick={() => this.simpleSynthesis(this.state.selectedNodeId)}>
            Simple synthesis
          </Button>
          <Button className="mr-3 p-1 btn-sm" onClick={() => this.smartBindSynthesisIO(this.state.selectedNodeId)}>
            Smart bind synthesis
          </Button>
        </div>
        <div className="d-flex flex-row-reverse my-auto">
          <Button className="mr-3 p-1 btn-sm" onClick={() => this.allBestThread(this.state.selectedNodeId, 2)}>
            All best tread 2
          </Button>
          <Button className="mr-3 p-1 btn-sm" onClick={() => this.allBestThread(this.state.selectedNodeId, 1)}>
            All best tread 1
          </Button>
          <Button className="mr-3 p-1 btn-sm" onClick={() => this.allBestThread(this.state.selectedNodeId, 0)}>
            Best tread
          </Button>
          <Button className="mr-3 p-1 btn-sm" onClick={() => this.obviousBindThread(this.state.selectedNodeId)}>
            Obvious bind thread
          </Button>
        </div>
      </div>
    );
  }
}
