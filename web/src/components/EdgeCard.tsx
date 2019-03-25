import * as React from "react";
import {Radar} from "react-chartjs-2";

interface EdgesCardProps {
    edge: Object;
}

interface EdgesCardState {
    selectedEdge: Object;
    eChar: number;
    tag: string;
    contentsEDecision: Object;
    contentsEOption: Object;
    eCharacteristics: Object;
}



export class EdgesCard extends React.Component<EdgesCardProps, EdgesCardState> {
    constructor (props: EdgesCardProps) {
        super(props);
        this.state = {
            selectedEdge: props.edge,
            eChar: props.edge.eCharacteristic,
            tag: props.edge.eDecision.tag.replace("Decision", ""),
            contentsEDecision: props.edge.eDecision.contents,
            contentsEOption: props.edge.eOption.contents,
            eCharacteristics: props.edge.eCharacteristics
        };
        this.reloadChart(props.edge);
    }

    componentWillReceiveProps(props: Object) {
        if (this.state.selectedEdge !== props.edge) this.reloadChart(props.edge);
        this.setState({selectedEdge: props.edge});
    }


    reloadChart(edge: Object) {
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
                    // borderColor: "rgba(220,220,220,1)",
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
                <div>
                    <h5><b>Previous Edge</b> [{this.state.eChar}]</h5>
                </div>
                <div>
                    <div>
                        <h6><b>tag: </b>{ this.state.tag }</h6>
                    </div>

                    <div>
                        <div>
                            <div style={{display: 'inline'}}>
                                <b>eDecision and </b>
                            </div>
                            <div className="hoverWrapper" style={{display: 'inline'}}>
                                <b style={{color: 'blue'}}>eOption:</b>
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
                        <b>&emsp;tag: </b>{String(this.state.eCharacteristics.tag)}
                        <br/>
                        <b>&emsp;isCritical: </b>{String(this.state.eCharacteristics.critical)}
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
            <div>
                <h5><b>Previous Edge</b> [{this.state.eChar}]</h5>
            </div>
            <div>
                <h6><b>tag: </b>{ this.state.tag }</h6>
            </div>
            <div>
                <div>
                    <div style={{display: 'inline'}}>
                        <b>eDecision and </b>
                    </div>
                    <div className="hoverWrapper" style={{display: 'inline'}}>
                        <b style={{color: 'blue'}}>eOption:</b>
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
                    <b>&emsp;isRestrictedTime: </b>{this.state.eCharacteristics.restrictedTime}
                    <br/>
                    <b>&emsp;WaitTime: </b>{this.state.eCharacteristics.waitTime}
                    <br/>
                    <b>&emsp;NotTransferableInputs: </b>{JSON.stringify(this.state.eCharacteristics.notTransferableInputs, null, 2)}
                </p>
            </div>
        </div>
        );
    }

    render() {

        if (String(this.state.contentsEDecision) === String(this.state.contentsEOption)) {
            return(
                    <div>{ this.renderBinding() }</div>
                );
        } else {
            return(
                <div>{ this.renderDataFlow() }</div>
            );
        }
    }
}