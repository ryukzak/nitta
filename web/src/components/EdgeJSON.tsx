import * as React from "react";

interface EdgesCardProps {
    edge: any;
}

interface EdgesCardState {
    selectedEdge: any;
    isHiddenEdgeJSON: any;
}

export class EdgeJSON extends React.Component<EdgesCardProps, EdgesCardState> {

    constructor (props: EdgesCardProps) {
        super(props);
        this.state = {
            selectedEdge: props.edge,
            isHiddenEdgeJSON: false
        };
        this.toggleDiv = this.toggleDiv.bind(this);
        this.reloadChart(props.edge);
    }

    componentWillReceiveProps(props: any) {
        if (this.state.selectedEdge !== props.edge) this.reloadChart(props.edge);
        this.setState({selectedEdge: props.edge});
    }

    toggleDiv = () => {
        this.setState({
            isHiddenEdgeJSON: !this.state.isHiddenEdgeJSON
        });
    }


    reloadChart(edge: any) {
        this.setState({
            selectedEdge: edge
        });
    }

    render() {
        return (
            <div>
                <pre>
                [<a onClick={ this.toggleDiv }> show/hide JSON </a>]
                </pre>
                { this.state.isHiddenEdgeJSON && 
                <div >
                    {/* <pre>previous edge:</pre> */}
                    <small><pre>{ JSON.stringify(this.state.selectedEdge, null, 2) }</pre></small>
                </div>
                }
            </div>
        );
    }
}