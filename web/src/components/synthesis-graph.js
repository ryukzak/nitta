import React, { Component } from 'react'
import 'react-table/react-table.css'
import { hapi } from '../hapi'
import Tree from 'react-d3-tree'

export class SynthesisGraph extends Component {
  constructor (props) {
    super(props)
    this.propagateSRoot = props.propagateSRoot

    this.state = {
      graph: null,
      nids: null,
      currentNid: null
    }
    this.refreshSynthesis()
  }

  componentWillReceiveProps (props) {
    console.debug('SynthesisGraph:componentWillReceiveProps() // props.currentNid, this.state.currentNid:', props.currentNid, this.state.currentNid)
    if (props.currentNid !== null && !(props.currentNid in this.state.nids)) this.refreshSynthesis()
    if (props.currentNid !== null && this.state.currentNid !== props.currentNid) {
      this.unmarkNode(this.state.currentNid)
      this.markNode(props.currentNid)
      this.setState({
        currentNid: props.currentNid,
        graph: [this.state.graph[0]] // force rerender Tree
      })
    }
  }

  markNode (nid, nids) {
    if (nids === undefined) nids = this.state.nids
    console.debug(nid)
    if (nid === null || nids === null) return
    nids[nid].nodeSvgShape = {
      shape: 'circle',
      shapeProps: {
        r: 10,
        cx: 0,
        cy: 0,
        fill: 'red'
      }
    }
  }

  unmarkNode (nid) {
    if (nid === null) return
    this.state.nids[nid].nodeSvgShape = undefined
  }

  refreshSynthesis () {
    console.debug('SynthesisGraph:refreshSynthesis()')
    hapi.getSynthesis()
      .then(response => {
        var i = 0
        var nids = {}
        var buildGraph = (gNode, dNode) => {
          gNode.name = i.toString()
          gNode.nid = dNode[0].svNnid
          nids[dNode[0].svNnid] = gNode
          i += 1
          gNode.attributes = {}
          dNode[0].svCntx.forEach(e => {
            gNode.attributes[e] = true
          })
          gNode.children = []
          dNode[1].forEach(e => {
            var tmp = {}
            gNode.children.push(tmp)
            buildGraph(tmp, e)
          })
          return gNode
        }
        var graph = buildGraph({}, response.data)
        nids['.'] = graph
        if (this.state.currentNid !== null) this.markNode(this.state.currentNid, nids)

        this.setState({
          graph: [graph],
          nids: nids
        })
      })
      .catch(err => console.log(err))
  }

  render () {
    if (this.state.graph === null) return <div />
    return (
      <div>
        <pre>
          [<a onClick={() => this.refreshSynthesis()}> refresh </a>]
          current synthesis (nid): {this.state.currentNid}
        </pre>
        <div style={{width: '100%', height: '300px', 'borderStyle': 'dashed', 'borderWidth': '1px'}}>
          <Tree
            data={this.state.graph}
            nodeSize={{x: 80, y: 80}}
            separation={{siblings: 1, nonSiblings: 1}}
            pathFunc='elbow'
            translate={{x: 20, y: 70}}
            collapsible={false}
            zoom={0.7}
            transitionDuration={0}
            styles={{nodes: {
              node: {
                name: {'fontSize': '12px'},
                attributes: {'fontSize': '10px'}
              },
              leafNode: {
                name: {'fontSize': '12px'},
                attributes: {'fontSize': '10px'}
              }
            }}}
            onClick={(node) => { console.debug('SynthesisGraph: propagateCurrentNid(', node.nid, ')'); this.propagateCurrentNid(node.nid) }}
          />
        </div>
      </div>
    )
  }
}
