import * as React from "react";
import { Button } from "react-bootstrap";
import SynthesisGraph from "./SynthesisGraph";
import AppContext from "../../app/AppContext";

export interface ITopButtonViewProps { }

export interface ITopButtonViewState {
    changedSynthesisGraphHeight: (newSynthesisGraphHeight: number) => void;
    synthesisGraphHeight: number;
    minSynthesisGraphHeight: number;
}

export default class TopButtonView extends React.Component<ITopButtonViewProps, ITopButtonViewState> {
    static contextType = AppContext;
    context!: React.ContextType<typeof AppContext>;

    private containerRef = React.createRef<HTMLDivElement>();
    private buttonRef = React.createRef<HTMLDivElement>();

    constructor(props: ITopButtonViewProps) {
        super(props);
        this.state = {
            synthesisGraphHeight: 0,
            minSynthesisGraphHeight: 0,
            changedSynthesisGraphHeight: (height: number) => { this.setState({ synthesisGraphHeight: height }) }
        };
    }

    componentDidMount = () => {
        if (this.containerRef.current != null && this.buttonRef.current != null && this.state.synthesisGraphHeight === 0) {
            let defaultHeight = this.containerRef.current.clientHeight - this.buttonRef.current.clientHeight;
            console.log(defaultHeight);
            this.state.changedSynthesisGraphHeight(defaultHeight);
            this.setState({ minSynthesisGraphHeight: defaultHeight });
        }
    }

    resizeSynthesisGraphView = (expand: boolean) => {
        if (expand) {
            this.state.changedSynthesisGraphHeight(this.state.synthesisGraphHeight + 100);
        } else if (this.state.synthesisGraphHeight > this.state.minSynthesisGraphHeight) {
            this.state.changedSynthesisGraphHeight(this.state.synthesisGraphHeight - 100);
        }
    }

    reload = () => {
        this.context.selectNode((this.context.selectedNodeId = null))
    }

    public render() {
        return (
            <div className="h-100" ref={this.containerRef}>
                <div className="d-flex justify-content-between my-1  mx-2" ref={this.buttonRef}>
                    <div className="d-flex flex-row my-auto">
                        <Button className="mr-3" onClick={() => this.resizeSynthesisGraphView(true)}>Expand</Button>
                        <Button className="mr-3" onClick={() => this.resizeSynthesisGraphView(false)}>Reduse</Button>
                        <Button className="mr-3" onClick={() => this.reload()}>Refresh</Button>
                    </div>
                    <div className="d-flex flex-row-reverse my-auto">
                        <h6>black - processed node; white - in progress node; green - succees synthesis</h6>
                    </div>
                </div>
                <div className="justify-content-center bg-light border p-2" style={{ height: this.state.synthesisGraphHeight }}>
                    <div className="h-100 d-flex align-items-center justify-content-center text-black-50">
                        <h1>Empty graph</h1>
                    </div>
                </div>
            </div>
        );
    }

}
