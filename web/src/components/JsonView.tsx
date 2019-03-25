import * as React from "react";

/**
 * Component to display JSON data. 
 * Takes two arguments: 
 * label - label of the json data that will be displayed on the button;
 * jsonData - data of json which will be displayed in the body of JsonView.
 */

interface JsonViewProps {
    jsonData: any;
    label: string;
}

interface JsonViewState {
    selectedLabel: string;
    selectedData: any;
    isHiddenJsonView: boolean;
}

export class JsonView extends React.Component<JsonViewProps, JsonViewState> {

    constructor (props: JsonViewProps) {
        super(props);
        this.state = {
            selectedLabel: props.label,
            selectedData: props.jsonData,
            isHiddenJsonView: false
        };
        this.toggleDiv = this.toggleDiv.bind(this);
        this.reloadChart(props.jsonData);
    }

    componentWillReceiveProps(props: JsonViewProps) {
        if (this.state.selectedData !== props.jsonData) this.reloadChart(props.jsonData);
        this.setState({selectedData: props.jsonData});
    }

    toggleDiv = () => {
        this.setState({
            isHiddenJsonView: !this.state.isHiddenJsonView
        });
    }


    reloadChart(jsonData: any) {
        this.setState({
            selectedData: jsonData
        });
    }

    render() {
        return (
            <div>
                <pre>
                [<a onClick={ this.toggleDiv }> show/hide {this.state.selectedLabel} </a>]
                </pre>
                { this.state.isHiddenJsonView && 
                <div >
                    <small><pre>{ JSON.stringify(this.state.selectedData, null, 2) }</pre></small>
                </div>
                }
            </div>
        );
    }
}