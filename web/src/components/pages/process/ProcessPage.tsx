import * as React from "react";
import { ProcessView } from "./ProcessView";
import { SynthesisHistoryView } from "./SynthesisHistoryView";
import {EndpointOptions} from "./EndpointOptios"

interface IProcessPageProps {
    nId: string | null;
}

interface IProcessPageState {
    nId: string | null;
}

export default class ProcessPage extends React.Component<IProcessPageProps, IProcessPageState>{
    // export default function NodePage() {
    constructor(props: IProcessPageProps) {
        super(props);

        this.state = {
            nId: props.nId
        }
    };

    componentWillReceiveProps(props: IProcessPageProps) {
        console.log("ProcessPage: change id = " + props.nId)
        if (this.state.nId !== props.nId) {
            this.setState({ nId: props.nId });
        }
    }

    render() {
        return (
            <div>
                {this.state.nId != null && (
                    <div className="d-flex flex-row m-2" >
                        <div className="col-md-6">
                            <ProcessView nId={this.state.nId} />
                        </div>
                        <div className="col-md-6">
                            Synthesis history :
                            <SynthesisHistoryView nId={this.state.nId} reverse={false} />
                            <br/>
                            EndpointOptions :
                            <EndpointOptions nId={this.state.nId} />
                        </div>
                    </div>
                )}
            </div>
        );
    }
}
