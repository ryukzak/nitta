import React, { Component } from 'react'
import 'react-table/react-table.css'
import { hapi } from '../hapi'
import { LinkButton } from '../utils'
import Tree from 'react-d3-tree'

export class SynthesisGraph extends Component {
  constructor (props) {
    super(props)
    this.propagateSRoot = props.propagateSRoot

    this.state = {
      synthesisNumber: null,
      graph: null
    }
    this.refreshSynthesis()
  }

  componentWillReceiveProps (props) {
    if (this.state.refreshTrigger !== props.refreshTrigger) this.refreshSynthesis()
  }

  refreshSynthesis () {
    hapi.getSynthesis()
      .then(response => {
        // var keys = []
        var key, info

        var graph = {}
        var cache = {}

        console.log(response.data)
        if (this.state.synthesisNumber === response.data.length) return

        response.data.forEach(item => {
          key = item[0]
          var sKey = showSRoot(key)
          info = item[1]
          var pointer
          if (info.siParent === null) {
            pointer = graph
            graph.style = {
              fill: 'red'
            }
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

        console.log(graph)
        this.setState({
          graph: [graph],
          synthesisNumber: response.data.length
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
          onClick={(node) => this.propagateSRoot(node.sRoot)}
        />
      </div>
    )
  }
}

// FIXME: dup
function showSRoot (sRoot) {
  console.log(sRoot)
  return sRoot.sid + '[' + sRoot.six + ']'
}
