import './App.css'
import 'react-table/react-table.css'
import React, { Component } from 'react'
import { SynthesisGraph } from './components/synthesis-graph'
import { SynthesisView } from './components/synthesis-view'

class App extends Component {
  constructor () {
    super()
    this.state = {
      currentNid: null,
      synthesisStatus: null
    }
  }

  onSynthesisChange (sNode) {
    console.debug('App:onSynthesisChange(', sNode, ')')
    if (sNode && sNode !== this.state.currentNid) {
      this.setState({
        currentNid: sNode
      })
    }
  }

  onSynthesisStatusChange (status) {
    console.debug('App:onSynthesisStatusChange(', status, ')')
    if (status && status !== this.state.currentNid) {
      this.setState({
        synthesisStatus: status
      })
    }
  }

  render () {
    return (
      <div>
        <SynthesisGraph currentNid={this.state.currentNid} onCurrentNidChange={nid => this.onSynthesisChange(nid)} onSynthesisStatusChange={status => this.onSynthesisStatusChange(status)} />
        <SynthesisView currentNid={this.state.currentNid} synthesisStatus={this.state.synthesisStatus} onCurrentNidChange={nid => this.onSynthesisChange(nid)} />
      </div>
    )
  }
}

export default App
