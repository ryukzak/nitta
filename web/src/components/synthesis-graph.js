import React, { Component } from 'react'
import 'react-table/react-table.css'
import { hapi } from '../hapi'
import { showSRoot } from '../utils'
import Tree from 'react-d3-tree'

export class SynthesisGraph extends Component {
  constructor (props) {
    super(props)
    this.propagateSRoot = props.propagateSRoot

    this.state = {
      synthesisNumber: null,
      sNode: null,
      graph: null,
      sNodes: null
    }
    this.refreshSynthesis()
  }

  componentWillReceiveProps (props) {
    console.debug('SynthesisGraph:componentWillReceiveProps() // state.sNode, props.sNode:', this.state.sNode, props.sNode)
    if (!this.state.sNodes[showSRoot(props.sNode)]) this.refreshSynthesis()
    if (this.state.sNode !== props.sNode) this.setState({sNode: props.sNode})
  }

  refreshSynthesis () {
    console.debug('SynthesisGraph:refreshSynthesis()')
    hapi.getSynthesis()
      .then(response => {
        var key, info
        var graph = {}
        var cache = {}
        var sNodes = {}

        if (this.state.synthesisNumber === response.data.length) return

        response.data.forEach(item => {
          key = item[0]
          var sKey = showSRoot(key)
          sNodes[sKey] = true
          info = item[1]
          var pointer
          if (info.siParent === null) {
            pointer = graph
          } else {
            pointer = cache[sKey]
          }
          pointer.name = key.sid
          pointer.sRoot = key
          pointer.attributes = {
            // steps: info.siSteps.length
          }
          // FIXME: Hightlight current synthesis.
          pointer.children = []
          // FIXME: sorting
          info.siChilds.forEach(e => {
            var node = {}
            cache[showSRoot(e)] = node
            pointer.children.push(node)
          })
        })

        this.setState({
          graph: [graph],
          synthesisNumber: response.data.length,
          sNodes: sNodes
        })
      })
      .catch(err => console.log(err))
  }

  render () {
    if (this.state.graph === null) return <div />
    // FIXME: dynamic width and size
    return (
      <div>
        <Tree data={this.state.graph}
          nodeSize={{x: 150, y: 50}}
          separation={{siblings: 1, nonSiblings: 1}}
          pathFunc='elbow'
          translate={{x: 20, y: 70}}
          collapsible={false}
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
          onClick={(node) => { console.debug('SynthesisGraph: propagateSRoot(', node.sRoot, ')'); this.propagateSRoot(node.sRoot) }}
        />
      </div>
    )
  }
}
