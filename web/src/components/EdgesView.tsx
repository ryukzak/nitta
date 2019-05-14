import * as React from "react";
import { haskellAPI } from "../middleware/haskell-api";
import ReactTable from "react-table";
import { LineChart } from "react-easy-chart";
import { JsonView } from "./JsonView";
import { GraphView } from "./GraphView";
import { EdgesCardsHolder } from "./EdgesCardsHolder";

interface EdgesViewProps {
    onNIdChange: any;
    selectedNId: any;
}

interface EdgesViewState {
    selectedNId: any;
    edge: any;
    options: any;
    options_raw: any; // FIXME: необходимо удалить
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
            options_raw: null,
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
        // FIXME: А зачем два раза спрашивать у сервера одно и тоже?
        if (nid === undefined || nid === null) return;
        console.debug("EdgesView:reloadEdges(", nid, ")");
        haskellAPI.getEdges(nid)
            .then((response: any) => {
                this.setState({
                    options: response.data,
                    options_raw: response.data.map(e => { return [e.eObjectiveFunctionValue, e.eParameters, e.eOption, e.eDecision]; })
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

    updateNid (i: number) {
        haskellAPI.getNode(this.state.selectedNId === ":" ? ":" + i : this.state.selectedNId + ":" + i)
            .then((response: any) => {
                this.onNIdChange(response.data.nId);
            })
            .catch((err: any) => alert(err));
    }

    render () {
        if (this.state.options === undefined || this.state.options === null) return <div />;

        var info = ""
        if (this.state.edge) info = JSON.stringify(this.state.edge.eDecision)
        return (
        <div>
            <div className="grid-x" >
                <div className="edgeGraphContainer" style={{"display": "inline-block", "width": "450px"}}>
                    <GraphView
                        selectedNId = { this.state.selectedNId }
                        view = "edges"
                    />
                </div>

                <div className="lineChartContainer">
                    <LineChart data={[ this.state.options_raw.map((e: any, index: any) => { return { x: index, y: e[0] }; }) ]}
                        width={500} height={250}
                    axes />
                </div>
                <div className="jsonViewContainer" style={{"display": "inline-block", "vertical-align": "top", "width": "270px"}}>
                    <JsonView jsonData={this.state.edge} label={"previous edge"} show={false} />
                </div>
            </div>
            <pre>{ info }</pre>
            <br/>
            <BindTable 
                name="Bind" 
                data={ this.state.options.filter( e => e.eParameters.tag === 'BindCh') } 
                updateNid={ i => { this.updateNid(i) } }
                />
            <DataflowTable 
                name="Transfers (DataFlow)" 
                data={ this.state.options.filter( e => e.eParameters.tag === 'DFCh') } 
                updateNid={ i => { this.updateNid(i) } }
                />
            <GenericTable
                name="Other" 
                data={ this.state.options.filter( e => e.eParameters.tag != 'BindCh' && e.eParameters.tag != 'DFCh') } 
                updateNid={ i => { this.updateNid(i) } }
                />
        </div>
        );
    }
}

function BindTable(props: {name: string, data: any[], updateNid: (nId: number) => void}) {
    var data = props.data
    return (
        <small>
            <pre>{ props.name }</pre>
            <ReactTable
                defaultPageSize={ data.length }
                minRows={ data.length }  
                showPagination={ false }
                columns={
                    [
                        {
                            Header: "Integral",
                            accessor: "eObjectiveFunctionValue",
                            maxWidth: 60,
                            Cell: row => 
                                <a onClick={() => {
                                    props.updateNid(row.index)
                                }}> { row.value } </a>
                        },
                        { Header: "PU", maxWidth: 80, Cell: (r: any) => {
                            var o = r.original.eOption.contents
                            return (<pre> { o[1] } </pre> )
                        }},
                        { Header: "Function", maxWidth: 300, Cell: (r: any) => {
                            var o = r.original.eOption.contents
                            return (<pre> { o[0] } </pre> )
                        }},
                        { Header: "Metrics", Cell: (r: any) => {
                            return (<pre> { JSON.stringify(r.original.eParameters) } </pre> )
                        }}
                    ]
                }
                data={ data } />
            <br/>
        </small>
    )
}

function DataflowTable(props: {name: string, data: any[], updateNid: (nId: number) => void}) {
    var data = props.data.map( e => { return e })
    return (
        <small>
            <pre>{ props.name }</pre>
            <ReactTable
                defaultPageSize={ data.length }
                minRows={ data.length }  
                showPagination={ false }
                columns={
                    [
                        {
                            Header: "Integral",
                            accessor: "eObjectiveFunctionValue",
                            maxWidth: 70,
                            Cell: row => 
                                <a onClick={() => {
                                    props.updateNid(row.index)
                                }}> { row.value } </a>
                        },
                        { Header: "Source", maxWidth: 80, Cell: (r: any) => {
                            var source = r.original.eOption.contents[0][0]
                            return (<pre> { source } </pre> )
                        }},
                        { Header: "Targets", maxWidth: 500, Cell: (r: any) => {
                            var o = r.original.eOption.contents
                            var targets = Object.keys(o[1]).map( k => { return k + ' <' + o[1][k][0] + '>' } ).join('; ')
                            return ( <pre> { targets } </pre> )
                        }},
                        { Header: "Metrics", Cell: (r: any) => {
                            return ( <pre> { JSON.stringify(r.original.eParameters) } </pre>)
                        }}
                    ]
                }
                data={ data } />
            <br/>
        </small>
    )
}

function GenericTable(props: {name: string, data: any[], updateNid: (nId: number) => void}) {
    var data = props.data.map( e => { return e })
    return (
        <small>
            <pre>{ props.name }</pre>
            <ReactTable
                defaultPageSize={ data.length }
                minRows={ data.length }  
                showPagination={ false }
                columns={
                    [
                        {
                            Header: "Integral",
                            accessor: "eObjectiveFunctionValue",
                            maxWidth: 70,
                            Cell: row => 
                                <a onClick={() => {
                                    props.updateNid(row.index)
                                }}> { row.value } </a>
                        },
                        { Header: "Options", Cell: (r: any) => {
                            return (<pre> { JSON.stringify(r.original.eOption) } </pre> )
                        }},
                        { Header: "Metrics", Cell: (r: any) => {
                            return ( <pre> { JSON.stringify(r.original.eParameters) } </pre>)
                        }}
                    ]
                }
                data={ data } />
            <br/>
        </small>
    )
}
