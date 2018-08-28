import React, { Component } from 'react'
import './App.css'
import 'react-table/react-table.css'
import { SynthesisGraph } from './components/synthesis-graph'
import { hapi } from './hapi'
import { LinkButton, showSRoot } from './utils'
import { ProcessView } from './components/process-view'

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
        <SynthesisGraph currentNid={this.state.currentNid} propagateCurrentNid={nid => this.onSynthesisChange(nid)} />
        <SynthesisView currentNid={this.state.currentNid} propagateCurrentNid={nid => this.onSynthesisChange(nid)} />
      </div>
    )
  }
}

class SynthesisView extends Component {
  constructor (props) {
    super(props)
    this.state = {
      currentNid: props.currentNid,
      view: 'update',
      model: null
    }
    this.propagateCurrentNid = props.propagateCurrentNid
    this.handleViewChange(props.currentNid, 'model')
  }

  handleViewChange (nid, view) {
    console.debug('SynthesisView:handleViewChange(', nid, view, ') // this.state.view:', this.state.view)
    if (nid === undefined || nid === null) return

    this.setState({currentNid: nid, view: 'update'})
    if (view === 'process') this.updateModel(nid, 'process')
    if (view === 'model') this.updateModel(nid, 'model')
  }

  componentWillReceiveProps (props) {
    var view = this.state.view
    if (view === 'update') view = 'model'
    if (this.state.currentNid !== props.currentNid) this.handleViewChange(props.currentNid, view)
  }

  compilerStep (oneStep) {
    hapi.compilerStep(this.state.currentNid, oneStep)
      .then(response => {
        this.handleViewChange(response.data[0], 'step', response.data[1])
      })
      .catch(err => alert(err))
  }

  updateModel (nid, view) {
    hapi.getModel(nid)
      .then(response => {
        this.setState({
          model: response.data,
          view: view
        })
      })
      .catch(err => console.log(err))
  }

  render (props) {
    return (
      <div>
        { this.state.currentNid === null && <pre> synthesis is not selected </pre> }

        { this.state.currentNid !== null &&
          <div>
            <div className='tiny button-group'>
              <a className='button primary' onClick={() => this.handleViewChange(this.state.currentNid, 'model')}>raw model</a>
              <a className='button primary' onClick={() => this.handleViewChange(this.state.currentNid, 'process')}>process</a>
              <a className='button primary' onClick={() => { this.compilerStep(true) }}>one step</a>
              <a className='button primary' onClick={() => { this.compilerStep(false) }}>all steps</a>
            </div>
            { this.state.view === 'update' && <pre> updating... </pre> }
            { this.state.view === 'model' && <pre> { JSON.stringify(this.state.model, null, 2) } </pre> }
            { this.state.view === 'process' && <ProcessView
              steps={this.state.model.nitta.process.steps}
              relations={this.state.model.nitta.process.relations}
            /> }

          </div>
        }

        {/* { this.state.view === 'info' && <SynthesisInfo
          data={this.state.data}
          propagateCurrentNid={currentNid => this.propagateCurrentNid(currentNid)}
        /> }
        { this.state.view === 'step' &&
          <StepView currentNid={this.state.currentNid} six={this.state.six} propagateCurrentNid={currentNid => this.propagateSRoot(currentNid)} />
        } */}
      </div>
    )
  }
}

function SynthesisInfo (props) {
  return (
    <dl>
      <dt>Parent:</dt>
      <dd> { props.data.sParent
        ? (<LinkButton sname={showSRoot(props.data.sParent)} onClick={() => props.propagateSRoot(showSRoot(props.data.sParent))} />)
        : (<div> - </div>) }
      </dd>
      <dt>Childs:</dt>
      <dd>
        <div className='tiny button-group'>
          { props.data.sChilds.map((k, i) => <LinkButton key={i} sname={showSRoot(k)} onClick={() => props.propagateSRoot(k)} />) }
        </div>
      </dd>
      <hr />
      <dd>
        <pre>{ JSON.stringify(props.data, null, 2) }</pre>
      </dd>
    </dl>
  )
}

export default App
