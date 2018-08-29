import './App.css'
import 'react-table/react-table.css'
import React, { Component } from 'react'
import { SynthesisGraph } from './components/synthesis-graph'
import { SynthesisView } from './components/synthesis-view'

class App extends Component {
  constructor () {
    super()
    this.state = {
      currentNid: null
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

  render () {
    return (
      <div>
        <SynthesisGraph currentNid={this.state.currentNid} onCurrentNidChange={nid => this.onSynthesisChange(nid)} />
        <SynthesisView currentNid={this.state.currentNid} onCurrentNidChange={nid => this.onSynthesisChange(nid)} />
      </div>
    )
  }
}

export default App
