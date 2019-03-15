import * as React from "react";

interface EdgesCardProps {
    jsonData: any;
    label: any;
}

interface EdgesCardState {
    selectedLabel: any;
    selectedEdge: any;
    isHiddenEdgeJSON: any;
}

export class JsonView extends React.Component<EdgesCardProps, EdgesCardState> {

    constructor (props: EdgesCardProps) {
        super(props);
        this.state = {
            selectedLabel: props.label,
            selectedEdge: props.jsonData,
            isHiddenEdgeJSON: false
        };
        this.toggleDiv = this.toggleDiv.bind(this);
        this.reloadChart(props.jsonData);
    }

    componentWillReceiveProps(props: any) {
        if (this.state.selectedEdge !== props.jsonData) this.reloadChart(props.jsonData);
        this.setState({selectedEdge: props.jsonData});
    }

    toggleDiv = () => {
        this.setState({
            isHiddenEdgeJSON: !this.state.isHiddenEdgeJSON
        });
    }


    reloadChart(jsonData: any) {
        this.setState({
            selectedEdge: jsonData
        });
    }

    render() {
        return (
            <div>
                <pre>
                [<a onClick={ this.toggleDiv }> show/hide {this.state.selectedLabel} </a>]
                </pre>
                { this.state.isHiddenEdgeJSON && 
                <div >
                    <small><pre>{ JSON.stringify(this.state.selectedEdge, null, 2) }</pre></small>
                </div>
                }
            </div>
        );
    }
}