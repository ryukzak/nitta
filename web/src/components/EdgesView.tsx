import * as React from "react";
import { haskellAPI } from "../middleware/haskell-api";
import ReactTable from "react-table";
import { LineChart } from "react-easy-chart";
import { EdgesCard } from "./EdgeCard";

interface EdgesViewProps {
    onNIdChange: any;
    selectedNId: any;
    // synthesisStatus: any;
}

interface EdgesViewState {
    selectedNId: any;
    edge: any;
    options: any;
    isDataResived: boolean;
}

export class EdgesView extends React.Component<EdgesViewProps, EdgesViewState> {

    onNIdChange: any;

    constructor (props: EdgesViewProps) {
        super(props);
        this.onNIdChange = props.onNIdChange;
        this.state = {
            selectedNId: props.selectedNId,
            options: null,
            edge: null,
            isDataResived: false,
        };
        this.reloadEdges(props.selectedNId);
    }

    componentWillReceiveProps (props: any) {
        console.debug("EdgesView:componentWillReceiveProps(", props, ")");
        if (this.state.selectedNId !== props.selectedNId) this.reloadEdges(props.selectedNId);
        this.setState({selectedNId: props.selectedNId});
    }

    reloadEdges (nid: any) {
        if (nid === undefined || nid === null) return;
        console.debug("EdgesView:reloadEdges(", nid, ")");
        haskellAPI.getEdges(nid)
        .then((response: any) => {
            this.setState({
                options: response.data.map(e => { return [e.eCharacteristic, e.eCharacteristics, e.eOption, e.eDecision]; })
            });
        })
        .catch(err => console.log(err));
        haskellAPI.getEdge(nid)
        .then((response: any) => {
            this.setState({
                edge: response.data,
                isDataResived: true
            });
        })
        .catch(err => console.log(err));
    }

    renderPreviousEdge () {
        return (
            <div>
            <pre>previous edge:</pre>
            <small><pre>{ JSON.stringify(this.state.edge, null, 2) }</pre></small>
            </div>
        );
    }

    render () {
        if (this.state.options === undefined || this.state.options === null) return <div />;
        if (this.state.options.length === 0) {
            return (
                <div>
                    { this.renderPreviousEdge() }
                    <pre> Process is over. Options not allow. </pre>
                </div>
            );
        }

        return (
        <div>
            <div className="grid-x">
                <div className="cell small-4">
                    { this.renderPreviousEdge() }
                </div>
                <div className="cell small-8">
                    <LineChart data={[ this.state.options.map((e: any, index: any) => { return { x: index, y: e[0] }; }) ]}
                    width={750} height={250}
                    axes />
                </div>
            </div>
            <div>
                {
                    this.state.isDataResived &&
                    <EdgesCard edge = {this.state.edge}/>
                }
            </div>
            <ReactTable
                columns={
                    [
                        {
                            Header: "Integral",
                            accessor: "0",
                            maxWidth: 70,
                            Cell: row =>
                            <a onClick={() => {
                                haskellAPI.getNode(this.state.selectedNId === ":" ? ":" + row.index : this.state.selectedNId + ":" + row.index)
                                .then((response: any) => {
                                    this.onNIdChange(response.data.nId);
                                })
                                .catch((err: any) => alert(err));
                            }}> { row.value }
                            </a>
                        },
                        {Header: "Description", accessor: "2", Cell: (row: any) => <pre> { JSON.stringify(row.value) } </pre>},
                        {Header: "Metrics", accessor: "1", Cell: (row: any) => <pre> { JSON.stringify(row.value) } </pre>}
                    ]
                }
                data={ this.state.options }
            />
        </div>
        );
    }
}
