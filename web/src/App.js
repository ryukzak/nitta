import './App.css'
import 'react-table/react-table.css'
import React, { Component } from 'react'
import { SynthesisGraph } from './components/synthesis-graph'
import { NodeView } from './components/node-view'

class App extends Component {
  constructor () {
    super()
    this.state = {
      selectedNId: null
    }
  }

  onNIdChange (nId) {
    console.debug('App:onNIdChange(', nId, ')')
    if (nId === 'reload') {
      this.setState({selectedNId: this.state.selectedNId})
      return
    }
    if (nId && nId !== this.state.selectedNId) {
      this.setState({selectedNId: nId})
    }
  }

  render () {
    return (
      <div>
        <SynthesisGraph
          selectedNId={this.state.selectedNId}
          onNIdChange={nid => this.onNIdChange(nid)}
        />
        <NodeView
          selectedNId={this.state.selectedNId}
          onNIdChange={nid => this.onNIdChange(nid)}
        />
      </div>
    )
  }
}

export default App
