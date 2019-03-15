import React, { Component } from 'react'
import 'react-table/react-table.css'
import { haskellAPI } from "../middleware/haskell-api";
import Graph from "react-graph-vis";


export class GraphView extends Component {

    constructor (props) {
        super(props)
        this.state = {
          selectedNId: props.selectedNId,
          view: props.view,
          status: false,

        events: {
            select: function(event) {
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

      componentWillReceiveProps (props) {
        if (this.state.selectedNId !== props.selectedNId) {
            this.setState({status: false})
            this.setState({view: props.view})
            this.graphMaker(props.selectedNId);
        }
    }

      graphUpdate(nid){
        haskellAPI.getEdge(nid)        
        .then( response => {
          var data = response.data;
          if( data.eCharacteristics !== null){
            var tag = data.eCharacteristics.tag;
            if(tag === 'BindCh'){
              var label = data.eDecision.contents[0];
              var act = this;
              this.state.graph.nodes.map(function(anObjectMapped, index){
                if( anObjectMapped.label === label ){
                    anObjectMapped.shadow = { enabled: true, color: '#efe300', x: 3, y: 3, size: 10};
                }
              })
            } else if( tag === 'DFCh' || tag === 'Refactorch'){
              var label = Object.keys(data.eDecision.contents[1]);
              var act = this;
              label.map(function(anObjectMapped, index){
                act.state.graph.edges.map(function(anObjectMapped2, index2){
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
        .catch(err => alert(err))
      }
    
    graphMaker(nid){
        haskellAPI.simpleSynthesisGraph(nid)
        .then(response => {
          var newNid = response.data
          var act = this;
          newNid.nodes.map(function(anObjectMapped, index) {
            act.state.graph.nodes[index] = {id: index+1, label: String(anObjectMapped.color), color:anObjectMapped.label}
          })
          newNid.edges.map(function(anObjectMapped, index){
            act.state.graph.edges[index] = ({from: anObjectMapped.from, to: anObjectMapped.to, label: anObjectMapped.label})
          })

          if( this.state.view === " synthesisNode" ){
            this.setState( {status: true} )
          }
          else if( this.state.view === " edges" )
            this.graphUpdate(nid)
        })
        .catch(err => alert(err)) 
        
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