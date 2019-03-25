import * as React from "react";
import 'react-table/react-table.css'
import { haskellAPI } from "../middleware/haskell-api";
import Graph from "react-graph-vis";

/**
 * Component to display algorihm graph. 
 * Takes two arguments: 
 * selectedNID - the node id that was selected;
 * view - the current view of program that determines the operation of the graph. 
 * (Takes two kinds of view: "edges" or "synthesisNode")
 */

interface GraphViewProps {
    selectedNId: number;
    view: string;
}

interface GraphViewState {
    selectedNId: number;
    view: string;
    status: boolean;
    events: any;
    options: any;
    graph: any;
}

export class GraphView extends React.Component<GraphViewProps, GraphViewState> {

    constructor (props: GraphViewProps) {
        super(props)
        this.state = {
          selectedNId: props.selectedNId,
          view: props.view,
          status: false,

        events: {
            select: function(event: any) {
                var { nodes, edges } = event;
            }
        },

          options: {
            layout: {  hierarchical: false },
            edges: {  color: "#000000" },
            nodes: {},
            physics: { enabled: false }
          },

          graph: {
            nodes: [],
            edges: []
          }
          
        }
          this.graphMaker(props.selectedNId);
      }

      componentWillReceiveProps (props: GraphViewProps) {
        if (this.state.selectedNId !== props.selectedNId) {
            this.setState({status: false})
            this.setState({view: props.view})
            this.graphMaker(props.selectedNId);
        }
    }

      graphUpdate(nid: number){
        haskellAPI.getEdge(nid)        
        .then( (response: any) => {
          let data = response.data;
          if( data.eCharacteristics !== null){
            let tag = data.eCharacteristics.tag;
            if(tag === "BindCh"){
              let label = data.eDecision.contents[0];
              this.state.graph.nodes.map(function(anObjectMapped: any, index: number){
                if( anObjectMapped.label === label ){
                    anObjectMapped.shadow = { enabled: true, color: '#efe300', x: 3, y: 3, size: 10};
                }
              })
            } else if( tag === "DFCh" || tag === "Refactorch"){
              let label = Object.keys(data.eDecision.contents[1]);
              let act = this;
              label.map(function(anObjectMapped, index){
                act.state.graph.edges.map(function(anObjectMapped2: any, index2: number){
                  if(anObjectMapped2.label === ("\""+anObjectMapped+"\"")){
                    anObjectMapped2.width = 3;
                    anObjectMapped2.shadow = { enabled: true, color: '#efe300', x: 3, y: 3, size: 10};
                  }
                })
              })
            }
            this.setState( {status: true} )
          }
        })
        .catch((err: any) => alert(err))
      }
    
    graphMaker(nid: number){
        haskellAPI.simpleSynthesisGraph(nid)
        .then((response: any) => {
          let newNid = response.data
          let act = this;
          newNid.nodes.map(function(anObjectMapped: any, index: number) {
            act.state.graph.nodes[index] = {id: index+1, label: String(anObjectMapped.color), color:anObjectMapped.label}
          })
          newNid.edges.map(function(anObjectMapped: any, index:number){
            act.state.graph.edges[index] = ({from: anObjectMapped.from, to: anObjectMapped.to, label: anObjectMapped.label})
          })

          if( this.state.view === "synthesisNode" ){
            this.setState( {status: true} )
          }
          else if( this.state.view === "edges" )
            this.graphUpdate(nid)
        })
        .catch((err: any) => alert(err)) 
      }
    

      render () {
        return (
          <div >
            { this.state.status === true &&
                <Graph graph={this.state.graph} options={this.state.options} events={this.state.events} style={{ height: "400px" }} />
            }
           </div>
        )
        }

}