import * as React from "react";
import { NodeView } from "../components/NodeView";
import { SynthesisGraph } from "../components/SynthesisGraph";
import "./../assets/scss/App.scss";
import "jquery/dist/jquery.js";
import "foundation-sites/dist/css/foundation.css";
import "foundation-sites/dist/js/foundation.js";

const nInSeparator = "-";

interface AppState {
    selectedNId: any;
}

export default class App extends React.Component<any, AppState> {
    constructor (props: any) {
        super(props);
        this.state = {
            selectedNId: nInSeparator
        };
    }

    onNIdChange (nId) {
        console.debug("App:onNIdChange(", nId, ")");
        if (nId === "reload") {
            this.setState({selectedNId: this.state.selectedNId});
            return;
        }
        if (nId && nId !== this.state.selectedNId) {
            this.setState({ selectedNId: nId });
        }
    }

    render () {
        return (
            <div>
                <SynthesisGraph
                    selectedNId={ this.state.selectedNId }
                    onNIdChange={ (nid: any) => this.onNIdChange(nid) }
                />
                <NodeView
                    selectedNId={ this.state.selectedNId }
                    onNIdChange={ (nid: string) => this.onNIdChange(nid) }
                />
            </div>
        );
    }
}
