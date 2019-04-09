import * as React from "react";
import {Radar} from "react-chartjs-2";

interface EdgesCardProps {
    nid: string;
    edge: JSON;
    maxValue: number;
}

interface EdgesCardState {
    selectedNId: string;
    selectedEdge: JSON;
    maxValue: number;
    eChar: number;
    tag: string;
    contentsEDecision: JSON;
    contentsEOption: JSON;
    eCharacteristics: JSON;
    isShown: boolean;
}



export class EdgesCard extends React.Component<EdgesCardProps, EdgesCardState> {
    constructor (props: EdgesCardProps) {
        super(props);
        this.state = {
            selectedNId: props.nid,
            selectedEdge: props.edge,
            maxValue: props.maxValue,
            eChar: props.edge.eCharacteristic,
            tag: props.edge.eDecision.tag.replace("Decision", ""),
            contentsEDecision: props.edge.eDecision.contents,
            contentsEOption: props.edge.eOption.contents,
            eCharacteristics: props.edge.eCharacteristics,
            isShown: false,
        };
        this.reloadChart(props.edge);
    }

    componentWillReceiveProps(props: EdgesCardProps) {
        if (this.state.selectedEdge !== props.edge) this.reloadChart(props.edge);
        this.setState({selectedEdge: props.edge});
    }

    toggleDiv = () => {
        this.setState({
            isShown: !this.state.isShown
        });
    }

    reloadChart(edge: any) {
        this.setState({
            selectedEdge: edge,
            eChar: edge.eCharacteristic,
            tag: edge.eDecision.tag,
            contentsEDecision: edge.eDecision.contents,
            contentsEOption: edge.eOption.contents,
            eCharacteristics: edge.eCharacteristics
        });
    }

    renderBinding() {
        let data = {
            labels: [
                "Allow Data Flow",
                "Alternative",
                "Number Of Binded Functions",
                "Percent Of Binded Inputs",
                "Restless",
                "Wave"
            ],
            datasets: [
                {
                    label: "Value",
                    backgroundColor: "rgba(100,255,100,0.2)",
                    pointBackgroundColor: "rgba(0,0,0,1)",
                    data: [
                        this.state.eCharacteristics.allowDataFlow,
                        this.state.eCharacteristics.alternative,
                        this.state.eCharacteristics.numberOfBindedFunctions,
                        this.state.eCharacteristics.percentOfBindedInputs,
                        this.state.eCharacteristics.restless,
                        this.state.eCharacteristics.wave
                    ]
                  }
            ]
        };

        return(
            <div>
                {this.renderProgressBar()}

                <div>
                    <h5><b>Edge</b> [{this.state.eChar}] {this.state.selectedNId}</h5>
                </div>
                <div>
                    <div>
                        <h6><b>tag: </b>{ this.state.tag }</h6>
                    </div>

                    <div>
                        <div>
                            <div style={{display: "inline"}}>
                                <b>eDecision and </b>
                            </div>
                            <div className="hoverWrapper" style={{display: "inline"}}>
                                <b style={{color: "blue"}}>eOption:</b>
                                <div id="hoverShow1">
                                    <small>
                                        <pre>{ JSON.stringify(this.state.contentsEOption, null, 2) }</pre>
                                    </small>
                                </div>
                            </div>
                        </div>
                    </div>

                    <div>
                        <small>
                            <pre>{ JSON.stringify(this.state.contentsEDecision, null, 2) }</pre>
                        </small>
                    </div>
                </div>
                <br/>
                <div>
                    <h6><b>eCharacteristics:</b></h6>
                    <p>
                        <b>&emsp;tag: </b>{this.state.eCharacteristics.tag}
                        <br/>
                        <b>&emsp;isInternalLockPossible: </b>{String(this.state.eCharacteristics.critical)}
                        <br/>
                        <b>&emsp;isPossibleDeadlock </b>{String(this.state.eCharacteristics.possibleDeadlock)}
                    </p>
                    <Radar data={data} />
                </div>
            </div>
        );
    }

    renderDataFlow() {
        return (
            <div>
                {this.renderProgressBar()}
            <div>
                <h5><b>Edge</b> [{this.state.eChar}] {this.state.selectedNId}</h5>
            </div>
            <div>
                <h6><b>tag: </b>{ this.state.tag }</h6>
            </div>
            <div>
                <div>
                    <div style={{display: "inline"}}>
                        <b>eDecision and </b>
                    </div>
                    <div className="hoverWrapper" style={{display: "inline"}}>
                        <b style={{color: "blue"}}>eOption:</b>
                        <div id="hoverShow1">
                            <small>
                                 <pre>{ JSON.stringify(this.state.contentsEOption, null, 2) }</pre>
                            </small>
                        </div>
                    </div>
                </div>
            </div>

            <div>
                <small>
                    <pre>{ JSON.stringify(this.state.contentsEDecision, null, 2) }</pre>
                </small>
            </div>

            <br/>
            <div>
                <h6><b>eCharacteristics:</b></h6>
                <p>
                    {/* &emsp; - is tabulation */}
                    <b>&emsp;tag: </b>{this.state.eCharacteristics.tag}
                    <br/>
                    <b>&emsp;isRestrictedTime: </b>{String(this.state.eCharacteristics.restrictedTime)}
                    <br/>
                    <b>&emsp;WaitTime: </b>{this.state.eCharacteristics.waitTime}
                    <br/>
                    <b>&emsp;NotTransferableInputs: </b>{JSON.stringify(this.state.eCharacteristics.notTransferableInputs, null, 2)}
                </p>
            </div>
        </div>
        );
    }

    renderProgressBar() {
        return(
            <div className="edgeCardProgressBar">
                <meter
                    min={0}
                    low={this.state.maxValue * 0.25}
                    optimum={this.state.maxValue}
                    high={this.state.maxValue * 0.75}
                    max={this.state.maxValue}
                    value={this.state.eChar}
                />
                {/* <progress max={this.state.maxValue} value={this.state.eChar} /> */}
            </div>
        );
    }

    render() {
        if (this.state.isShown === false) {
            return (
                <div style={{"width": "10px", "height": "100%", "word-wrap": "break-word"}} onClick={this.toggleDiv} >
                    <h6>{this.state.selectedNId}</h6>
                </div>
            );
        } else if (String(this.state.contentsEDecision) === String(this.state.contentsEOption)) {
            return(
                    <div onClick={this.toggleDiv}>{ this.renderBinding() }</div>
            );
        } else {
            return(
                <div onClick={this.toggleDiv}>{ this.renderDataFlow() }</div>
            );
        }
    }
}