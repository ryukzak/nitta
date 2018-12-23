import React, { Component } from 'react'
import 'react-table/react-table.css'
import { hapi } from '../hapi'
import Tree from 'react-d3-tree'

export class SynthesisGraph extends Component {
  constructor (props) {
    super(props)
    this.onNIdChange = props.onNIdChange
    this.state = {
      selectedNId: null,
      graph: null,
      nIds: null,
      height: 200
    }
    this.reloadSynthesisGraph()
  }

  componentWillReceiveProps (props) {
    console.debug('SynthesisGraph:componentWillReceiveProps() // props.selectedNId, this.state.selectedNId:', props.selectedNId, this.state.selectedNId)

    if (props.selectedNId !== null && !(props.selectedNId in this.state.nIds)) {
      this.setState({
        selectedNId: props.selectedNId
      })
      this.reloadSynthesisGraph()
      return
    }
    if (props.selectedNId !== null && this.state.selectedNId !== props.selectedNId) {
      this.unmarkNode(this.state.selectedNId)
      this.markNode(props.selectedNId)
      this.setState({
        selectedNId: props.selectedNId,
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

  reloadSynthesisGraph () {
    console.debug('SynthesisGraph:reloadSynthesisGraph()')
    var reLastNidStep = /:[^:]*$/
    hapi.getSynthesis()
      .then(response => {
        var nIds = {}
        var buildGraph = (gNode, dNode) => {
          gNode.name = reLastNidStep.exec(dNode[0].svNnid)[0]
          gNode.nid = dNode[0].svNnid
          nIds[dNode[0].svNnid] = gNode
          if (dNode[0].svIsEdgesProcessed) this.markNode(gNode.nid, nIds, 'black')
          if (dNode[0].svIsComplete) this.markNode(gNode.nid, nIds, 'lime')
          gNode.attributes = { dur: dNode[0].svDuration, ch: dNode[0].svCharacteristic }
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
        if (this.state.selectedNId !== null) this.markNode(this.state.selectedNId, nIds)

        this.setState({
          graph: [graph],
          nIds: nIds
        })
      })
      .catch(err => console.log(err))
  }

  stepsNumber () {
    if (this.state.selectedNId === null) return 'NaN'
    if (this.state.selectedNId === ':') return 0
    return this.state.selectedNId.split(':').length - 1
  }

  render () {
    if (this.state.graph === null) return <div />
    return (
      <div>
        <pre>
          [<a onClick={() => this.setState({height: this.state.height + 100})}> expand </a>] /
          [<a onClick={() => this.setState({height: this.state.height - 100})}> reduce </a>]
          [<a onClick={() => this.reloadSynthesisGraph()}> refresh </a>]
          steps: {this.stepsNumber()}; selected synthesis nid - {this.state.selectedNId}
        </pre>
        <div style={{width: '100%', height: this.state.height + 'px', 'borderStyle': 'dashed', 'borderWidth': '1px'}}>
          <Tree
            data={this.state.graph}
            nodeSize={{x: 160, y: 60}}
            separation={{siblings: 1, nonSiblings: 1}}
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
              console.debug('SynthesisGraph: onNIdChange(', node.nid, ')')
              this.onNIdChange(node.nid)
            }}
          />
        </div>
        <pre className='text-right'>black - processed node; white - in progress node; green - success synthesis</pre>
      </div>
    )
  }
}
