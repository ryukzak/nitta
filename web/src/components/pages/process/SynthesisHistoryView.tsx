import * as React from "react";
import { haskellApiService } from "../../../services/HaskellApiService";

interface Props {
  nId: string;
  reverse: boolean;
}

interface State {
  nId: string | null;
  history: any[] | null;
}

export class SynthesisHistoryView extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = {
      nId: null,
      history: null
    };
  }

  static getDerivedStateFromProps(props: Props, state: State) {
    if (props.nId && props.nId !== state.nId) {
      return { nId: props.nId, history: null } as State;
    }
    return null;
  }

  componentDidMount() {
    this.updateHistory(this.state.nId!);
  }

  componentDidUpdate(prevProps: Props, prevState: State, snapshot: any) {
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
          history: response.data
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
