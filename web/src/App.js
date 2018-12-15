import './App.css'
import 'react-table/react-table.css'
import React, { Component } from 'react'
import { SynthesisGraph } from './components/synthesis-graph'
import { NodeView } from './components/node-view'

class App extends Component {
  constructor () {
    super()
    this.state = {
      currentNId: null,
      synthesisStatus: null
    }
  }

  onSynthesisChange (sNode) {
    console.debug('App:onSynthesisChange(', sNode, ')')
    if (sNode && sNode !== this.state.currentNId) {
      this.setState({
        currentNId: sNode
      })
    }
  }

  onSynthesisStatusChange (status) {
    console.debug('App:onSynthesisStatusChange(', status, ')')
    if (status && status !== this.state.currentNId) {
      this.setState({
        synthesisStatus: status
      })
    }
  }

  render () {
    return (
      <div>
        <SynthesisGraph currentNId={this.state.currentNId} onCurrentNIdChange={nid => this.onSynthesisChange(nid)} onSynthesisStatusChange={status => this.onSynthesisStatusChange(status)} />
        <NodeView currentNId={this.state.currentNId} synthesisStatus={this.state.synthesisStatus} onCurrentNIdChange={nid => this.onSynthesisChange(nid)} />
      </div>
    )
  }
}

export default App
