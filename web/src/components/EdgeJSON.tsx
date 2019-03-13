import * as React from "react";

interface EdgesCardProps {
    edge: any;
}

interface EdgesCardState {
    selectedEdge: any;
}



export class EdgeJSON extends React.Component<EdgesCardProps, EdgesCardState> {

    constructor (props: EdgesCardProps) {
        super(props);
        this.state = {
            selectedEdge: props.edge,
        };
        this.reloadChart(props.edge);
    }

    componentWillReceiveProps(props: any) {
        if (this.state.selectedEdge !== props.edge) this.reloadChart(props.edge);
        this.setState({selectedEdge: props.edge});
    }


    reloadChart(edge: any) {
        this.setState({
            selectedEdge: edge
        });
    }

    render() {
        return (
            <div>
            <pre>previous edge:</pre>
            <small><pre>{ JSON.stringify(this.state.selectedEdge, null, 2) }</pre></small>
            </div>
        );
    }
}