import * as React from "react";
import { Button } from "react-bootstrap";
import AppContext from "../../app/AppContext";

export interface ITopButtonViewProps { }
export interface ITopButtonViewState { }

export default class TopButtonView extends React.Component<ITopButtonViewProps, ITopButtonViewState> {
    static contextType = AppContext;
    context!: React.ContextType<typeof AppContext>;

    constructor(props: ITopButtonViewProps) {
        super(props);
        this.state = {};
    }

    resizeSynthesisGraphView = (expand: boolean) => {
        if (expand) {
            this.context.changedSynthesisGraphHeight((this.context.synthesisGraphHeight += 100));
        } else if (this.context.synthesisGraphHeight > this.context.minSynthesisGraphHeight) {
            this.context.changedSynthesisGraphHeight((this.context.synthesisGraphHeight -= 100));
        }
    }

    public render() {
        return (
            <div className="d-flex justify-content-between my-1  mx-2">
                <div className="d-flex flex-row my-auto">
                    <Button className="mr-3" onClick={() => this.resizeSynthesisGraphView(true)}>Expand</Button>
                    <Button className="mr-3" onClick={() => this.resizeSynthesisGraphView(false)}>Reduse</Button>
                </div>
                <div className="d-flex flex-row-reverse my-auto">
                    <h6>black - processed node; white - in progress node; green - succees synthesis</h6>
                </div>
            </div>
        );
    }

}
