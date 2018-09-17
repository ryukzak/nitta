import React, { Component } from 'react'
import 'react-table/react-table.css'
import { hapi } from '../hapi'
import Tree from 'react-d3-tree'

export class SynthesisGraph extends Component {
  constructor (props) {
    super(props)
    this.onCurrentNidChange = props.onCurrentNidChange
    this.onSynthesisStatusChange = props.onSynthesisStatusChange
    this.state = {
      graph: null,
      nids: null,
      currentNid: null
    }
    this.reloadSynthesis()
  }

  componentWillReceiveProps (props) {
    console.debug('SynthesisGraph:componentWillReceiveProps() // props.currentNid, this.state.currentNid:', props.currentNid, this.state.currentNid)

    if (props.currentNid !== null && !(props.currentNid in this.state.nids)) {
      this.setState({
        currentNid: props.currentNid
      })
      this.reloadSynthesis()
      return
    }
    if (props.currentNid !== null && this.state.currentNid !== props.currentNid) {
      this.unmarkNode(this.state.currentNid)
      this.markNode(props.currentNid)
      this.setState({
        currentNid: props.currentNid,
        graph: [this.state.graph[0]] // force rerender Tree
      })
    }
  }

  markNode (nid, nids, color) {
    if (color === undefined) color = 'blue'
    if (nids === undefined) nids = this.state.nids
    if (nid === null || nids === null) return

    if (color === 'blue') {
      nids[nid].nodeSvgShapeOriginal = nids[nid].nodeSvgShape
    }
    console.debug('SynthesisGraph:markNode(', nid, nids, color, ')')
    nids[nid].nodeSvgShape = {
      shape: 'circle',
      shapeProps: {
        r: 10,
        cx: 0,
        cy: 0,
        fill: color
      }
    }
  }

  unmarkNode (nid) {
    console.debug('SynthesisGraph:unmarkNode(', nid, ')')
    if (nid === null) return
    this.state.nids[nid].nodeSvgShape = this.state.nids[nid].nodeSvgShapeOriginal
  }

  reloadSynthesis () {
    console.debug('SynthesisGraph:reloadSynthesis()')
    var reLastNidStep = /:[^:]*$/
    hapi.getSynthesis()
      .then(response => {
        var nids = {}
        var buildGraph = (gNode, dNode) => {
          gNode.name = reLastNidStep.exec(dNode[0].svNnid)[0]
          gNode.nid = dNode[0].svNnid
          nids[dNode[0].svNnid] = gNode
          if (dNode[0].svStatus === 'Finished') this.markNode(gNode.nid, nids, 'lime')
          if (dNode[0].svStatus === 'DeadEnd') this.markNode(gNode.nid, nids, 'red')
          gNode.attributes = { dur: dNode[0].svDuration }
          gNode.status = dNode[0].svStatus
          dNode[0].svCntx.forEach((e, i) => {
            gNode.attributes[i] = e
          })
          gNode.children = []
          dNode[1].forEach(e => {
            var tmp = {}
            gNode.children.push(tmp)
            buildGraph(tmp, e)
          })
          console.log('>', dNode[0].svNnid, ' --> ', dNode[0].svStatus)

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

  stepsNumber () {
    if (this.state.currentNid === null) return 'NaN'
    if (this.state.currentNid === ':') return 0
    return this.state.currentNid.split(':').length - 1
  }

  render () {
    if (this.state.graph === null) return <div />
    return (
      <div>
        <pre>
          [<a onClick={() => this.reloadSynthesis()}> refresh </a>]
          steps: {this.stepsNumber()}; selected synthesis nid - {this.state.currentNid}
        </pre>
        <div style={{width: '100%', height: '200px', 'borderStyle': 'dashed', 'borderWidth': '1px'}}>
          <Tree
            data={this.state.graph}
            nodeSize={{x: 200, y: 80}}
            separation={{siblings: 1, nonSiblings: 1}}
            pathFunc='elbow'
            translate={{x: 20, y: 70}}
            collapsible={false}
            zoom={0.7}
            transitionDuration={0}
            nodeSvgShape={{
              shape: 'circle',
              shapeProps: {
                r: 10,
                cx: 0,
                cy: 0,
                fill: 'white'
              }
            }}
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
            onClick={(node) => {
              console.debug('SynthesisGraph: onCurrentNidChange(', node.nid, ')')
              this.onCurrentNidChange(node.nid)
              this.onSynthesisStatusChange(node.status)
            }}
          />
        </div>
        <pre class='text-right'>red - dead end; green - finished; blue - current</pre>
      </div>
    )
  }
}
