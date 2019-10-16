import * as React from "react";
import { Button } from "react-bootstrap";
import SynthesisGraph from "./SynthesisGraph"

export interface ISynthesisGraphViewProps {
    selectedNid: string | null,
    selectNode: (nid: string | null) => void,
    refreshGraph: () => void
}

export interface ISynthesisGraphViewState {
    selectedNid: string | null;
    selectNode: (nid: string | null) => void;
    refreshGraph: () => void;
    height: number;
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
            height: 0
        };
    }

    componentWillReceiveProps(newProps: ISynthesisGraphViewProps) {
        this.setState({
            selectedNid: newProps.selectedNid,
            selectNode: newProps.selectNode
        })
    }

    componentDidMount = () => {
        if (this.containerRef.current != null && this.buttonRef.current != null ) {
            this.setState({height: this.containerRef.current.clientHeight - this.buttonRef.current.clientHeight})  ; 
        }
    }

    public render() {
        return (
            <div className="flex-grow-1" ref={this.containerRef}>
                <div className="d-flex justify-content-between my-1  mx-2" ref={this.buttonRef}>
                    <div className="d-flex flex-row my-auto">
                        <Button className="btn btn-link bg-transparent mr-3" style={{ border: 0 }} onClick={() => this.state.refreshGraph()}>Refresh</Button>
                    </div>
                    <div className="d-flex flex-row-reverse my-auto">
                        <h6>black - processed node; white - in progress node; green - succees synthesis</h6>
                    </div>
                </div>
                <div className="justify-content-center bg-light border p-2 " style={{height: this.state.height}} >
                    <SynthesisGraph selectedNId={this.state.selectedNid} onNIdChange={this.state.selectNode} />
                </div>
            </div>
        );
    }

}
