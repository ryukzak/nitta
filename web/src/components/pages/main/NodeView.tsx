import * as React from "react";
import { Button } from "react-bootstrap";
import AppContext from "../../app/AppContext";

export interface IButtonViewProps { }
export interface IButtonViewState { }

export default class ButtonView extends React.Component<IButtonViewProps, IButtonViewState> {
    static contextType = AppContext;
    context!: React.ContextType<typeof AppContext>;

    constructor(props: IButtonViewProps) {
        super(props);
        this.state = {};
    }

    handleClick = (str: string) => {
        console.log("Label = " + str);
    }

    public render() {
        return (
            <div className="d-flex justify-content-between my-3  mx-2 pb-2">
                <div className="d-flex flex-row my-auto">
                    <Button className="mr-3" onClick={() => this.handleClick("some")}>SIMPLE SYNTHESIS</Button>
                    <Button className="mr-3" onClick={() => this.handleClick("some")}>SMART BIND SYNTHESIS</Button>
                </div>
                <div className="d-flex flex-row-reverse my-auto">
                    <Button className="mr-3" onClick={() => this.handleClick("some")}>OBVIOUS BIND THREAD</Button>
                    <Button className="mr-3" onClick={() => this.handleClick("some")}>BEST THREAD</Button>
                    <Button className="mr-3" onClick={() => this.handleClick("some")}>ALL BEST THREAD 1</Button>
                    <Button className="mr-3" onClick={() => this.handleClick("some")}>ALL BEST THREAD 2</Button>
                </div>
            </div>
        );
    }
}
