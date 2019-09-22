import * as React from "react";
import { haskellAPI } from "../middleware/haskell-api";
import { ProcessTimelines, ViewPointID, TimelinePoint, TimelineWithViewPoint } from "../gen/types";

interface Props {
    nId: string;
}

interface State {
    nId: string;
    history: any[];
}

export class SynthesisHistoryView extends React.Component<Props, State> {
    constructor(props: Props) {
        super(props);
        this.state = {
            nId: null,
            history: null,
        };
    }

    static getDerivedStateFromProps(props: Props, state: State) {
        if (props.nId && props.nId !== state.nId) {
            return { nId: props.nId, history: null } as State;
        }
        return null;
    }

    componentDidMount() {
        this.updateHistory(this.state.nId);
    }

    componentDidUpdate(prevProps: Props, prevState: State, snapshot: any) {
        if (prevState.nId !== this.state.nId) {
            this.updateHistory(this.state.nId);
        }
    }

    updateHistory(nid: string) {
        haskellAPI.getHistory(nid)
                  .then((response: any) => {
                      this.setState({
                          history: response.data
                      })
                  })
                  .catch((err: any) => console.log(err))
    }

    render() {
        if (this.state.history == null) return (<pre>LOADING...</pre>);
        // FIXME: history decisions should be view as in edges view
        return (
            <pre> {this.state.history.map((e, i) =>
                <div key={i}> { i + " - " }
                    { e.tag == "BindingView" && e.tag + " - " + e.pu + " - " + e.function }
                    { e.tag == "RefactorView" && e.tag + " - " + e.contents }
                    { e.tag == "DataflowView" && e.tag + " - " + JSON.stringify(e) }
                </div>
            )
            } </pre>
        )
    }

}
