import React, { Component } from 'react'
import 'react-table/react-table.css'
import { hapi } from '../hapi'
import Tree from 'react-d3-tree'

export class SynthesisGraph extends Component {
  constructor (props) {
    super(props)
    this.onCurrentNIdChange = props.onCurrentNIdChange
    this.onSynthesisStatusChange = props.onSynthesisStatusChange
    this.state = {
      graph: null,
      nIds: null,
      currentNId: null,
      height: 200
    }
    this.reloadSynthesis()
  }

  componentWillReceiveProps (props) {
    console.debug('SynthesisGraph:componentWillReceiveProps() // props.currentNId, this.state.currentNId:', props.currentNId, this.state.currentNId)

    if (props.currentNId !== null && !(props.currentNId in this.state.nIds)) {
      this.setState({
        currentNId: props.currentNId
      })
      this.reloadSynthesis()
      return
    }
    if (props.currentNId !== null && this.state.currentNId !== props.currentNId) {
      this.unmarkNode(this.state.currentNId)
      this.markNode(props.currentNId)
      this.setState({
        currentNId: props.currentNId,
        graph: [this.state.graph[0]] // force rerender Tree
      })
    }
  }

  markNode (nid, nIds, color) {
    if (color === undefined) color = 'blue'
    if (nIds === undefined) nIds = this.state.nIds
    if (nid === null || nIds === null) return

    if (color === 'blue') {
      nIds[nid].nodeSvgShapeOriginal = nIds[nid].nodeSvgShape
    }
    console.debug('SynthesisGraph:markNode(', nid, nIds, color, ')')
    nIds[nid].nodeSvgShape = {
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
    this.state.nIds[nid].nodeSvgShape = this.state.nIds[nid].nodeSvgShapeOriginal
  }

  reloadSynthesis () {
    console.debug('SynthesisGraph:reloadSynthesis()')
    var reLastNidStep = /:[^:]*$/
    hapi.getSynthesis()
      .then(response => {
        var nIds = {}
        var buildGraph = (gNode, dNode) => {
          gNode.name = reLastNidStep.exec(dNode[0].svNnid)[0]
          gNode.nid = dNode[0].svNnid
          nIds[dNode[0].svNnid] = gNode
          if (dNode[0].svIsComplete) this.markNode(gNode.nid, nIds, 'lime')
          gNode.attributes = { dur: dNode[0].svDuration }
          gNode.status = dNode[0].svIsComplete
          dNode[0].svCntx.forEach((e, i) => {
            gNode.attributes[i] = e
          })
          gNode.children = []
          dNode[1].forEach(e => {
            var tmp = {}
            gNode.children.push(tmp)
            buildGraph(tmp, e)
          })
          console.log('>', dNode[0].svNnid, ' --> ', dNode[0].svIsComplete)

          return gNode
        }
        var graph = buildGraph({}, response.data)
        nIds['.'] = graph
        if (this.state.currentNId !== null) this.markNode(this.state.currentNId, nIds)

        this.setState({
          graph: [graph],
          nIds: nIds
        })
      })
      .catch(err => console.log(err))
  }

  stepsNumber () {
    if (this.state.currentNId === null) return 'NaN'
    if (this.state.currentNId === ':') return 0
    return this.state.currentNId.split(':').length - 1
  }

  render () {
    if (this.state.graph === null) return <div />
    return (
      <div>
        <pre>
          [<a onClick={() => this.setState({height: this.state.height + 100})}> expand </a>] /
          [<a onClick={() => this.setState({height: this.state.height - 100})}> reduce </a>]
          [<a onClick={() => this.reloadSynthesis()}> refresh </a>]
          steps: {this.stepsNumber()}; selected synthesis nid - {this.state.currentNId}
        </pre>
        <div style={{width: '100%', height: this.state.height + 'px', 'borderStyle': 'dashed', 'borderWidth': '1px'}}>
          <Tree
            data={this.state.graph}
            nodeSize={{x: 200, y: 80}}
            separation={{siblings: 0.5, nonSiblings: 1}}
            pathFunc='diagonal'
            translate={{x: 20, y: 40}}
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
              console.debug('SynthesisGraph: onCurrentNIdChange(', node.nid, ')')
              this.onCurrentNIdChange(node.nid)
              this.onSynthesisStatusChange(node.status)
            }}
          />
        </div>
        <pre className='text-right'>red - dead end; green - finished; blue - current</pre>
      </div>
    )
  }
}
