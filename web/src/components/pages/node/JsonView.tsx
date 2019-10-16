import * as React from "react";

/**
 * Component to display JSON data.
 * Takes three arguments:
 * label - label of the json data that will be displayed on the button;
 * jsonData - data of json which will be displayed in the body of JsonView.
 * show - collapse or open body of JsonView by default
 */

interface JsonViewProps {
    jsonData: any;
    label: string;
    show: boolean;
}

interface JsonViewState {
    selectedData: any;
    selectedLabel: string;
    isHiddenJsonView: boolean;
}

export class JsonView extends React.Component<JsonViewProps, JsonViewState> {

    constructor (props: JsonViewProps) {
        super(props);
        this.state = {
            selectedLabel: props.label,
            selectedData: props.jsonData,
            isHiddenJsonView: props.show
        };
        this.toggleDiv = this.toggleDiv.bind(this);
        // FIXME:
        this.reloadChart(props.jsonData);
    }

    UNSAFE_componentWillReceiveProps(props: JsonViewProps) {
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
                [<a onClick={ this.toggleDiv }> JSON: {this.state.selectedLabel} </a>]
                </pre>
                {/* <input type="checkbox"/> */}
                { this.state.isHiddenJsonView &&
                <div className="jsonViewBody">
                    <small><pre>{ JSON.stringify(this.state.selectedData, null, 2) }</pre></small>
                </div>
                }
            </div>
        );
    }
}