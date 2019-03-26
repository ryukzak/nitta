import * as React from "react";
import { haskellAPI } from "../middleware/haskell-api";
import ReactTable from "react-table";
import { LineChart } from "react-easy-chart";
import { EdgesCard } from "./EdgeCard";
import { JsonView } from "./JsonView";
import { GraphView } from "./GraphView"
import { EdgesCardsHolder } from "./EdgesCardsHolder";

interface EdgesViewProps {
    onNIdChange: any;
    selectedNId: any;
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

    render () {
        if (this.state.options === undefined || this.state.options === null) return <div />;

        return (
        <div>
            <div className="grid-x" >
                <div className="edgeGraphContainer" style={{'display': "inline-block", 'width': '450px'}}>
                    <GraphView 
                        selectedNId = { this.state.selectedNId }
                        view = "edges"
                    />
                </div>
                
                <div className="lineChartContainer">
                    <LineChart data={[ this.state.options.map((e: any, index: any) => { return { x: index, y: e[0] }; }) ]}
                        width={500} height={250}
                    axes />
                </div>
                <div className="jsonViewContainer" style={{'display': "inline-block", 'vertical-align': 'top', 'width': '270px'}}>
                    <JsonView jsonData={this.state.edge} label={"previous edge"} show={false} />
                </div>
            </div>

            {
                this.state.isDataResived &&
                <div >
                    <EdgesCardsHolder nid = {this.state.selectedNId}  />
                </div>
            }

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
