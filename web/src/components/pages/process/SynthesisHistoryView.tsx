import * as React from "react";
import { haskellApiService } from "../../../services/HaskellApiService";

// TODO: REWRITE/REFACTOR COMPONENT "SynthesisHistoryView"

export interface ISynthesisHistoryViewProps {
  nId: string;
  reverse: boolean;
}

export interface ISynthesisHistoryViewState {
  nId: string | null;
  history: any[] | null;
}

export class SynthesisHistoryView extends React.Component<ISynthesisHistoryViewProps, ISynthesisHistoryViewState> {
  constructor(props: ISynthesisHistoryViewProps) {
    super(props);
    this.state = {
      nId: null,
      history: null,
    };
  }

  static getDerivedStateFromProps(props: ISynthesisHistoryViewProps, state: ISynthesisHistoryViewState) {
    if (props.nId && props.nId !== state.nId) {
      return { nId: props.nId, history: null } as ISynthesisHistoryViewState;
    }
    return null;
  }

  componentDidMount() {
    this.updateHistory(this.state.nId!);
  }

  componentDidUpdate(prevProps: ISynthesisHistoryViewProps, prevState: ISynthesisHistoryViewState, snapshot: any) {
    console.log("histiory CDU: new id = " + prevProps.nId);
    if (prevState.nId !== this.state.nId) {
      this.updateHistory(this.state.nId!);
    }
  }

  updateHistory(nid: string) {
    haskellApiService
      .getHistory(nid)
      .then((response: any) => {
        this.setState({
          history: response.data,
        });
      })
      .catch((err: any) => console.log(err));
  }

  render() {
    if (this.state.history == null) return <pre>LOADING...</pre>;
    let history = this.props.reverse ? this.state.history.reverse() : this.state.history;
    // FIXME: history decisions should be view as in edges view
    return (
      <pre className="squeeze">
        {" "}
        {history.map((e, i) => (
          <div key={i}>
            {" "}
            {(this.props.reverse ? history.length - i - 1 : i) + " - "}
            {e.tag === "BindingView" && e.tag + " | " + e.pu + " <- " + e.function}
            {e.tag === "RefactorView" && e.tag + " - " + e.contents}
            {e.tag === "DataflowView" && e.tag + " - " + JSON.stringify(e)}
          </div>
        ))}{" "}
      </pre>
    );
  }
}
