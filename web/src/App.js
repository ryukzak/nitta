import React, { Component } from 'react'
import './App.css'
import 'react-table/react-table.css'
import { SynthesisGraph } from './components/synthesis-graph'
import { hapi } from './hapi'
import { LinkButton, showSRoot } from './utils'
import { ProcessView } from './components/process-view'
import ReactTable from 'react-table'
import { LineChart } from 'react-easy-chart'
// import { StepOptionView } from './components/step-view'

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
      model: null,
      scOptions: null
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
    if (view === 'scOptions') this.setState({view: 'scOptions'})
  }

  componentWillReceiveProps (props) {
    console.debug('SynthesisView:componentWillReceiveProps(', props, ')')
    var view = this.state.view
    if (view === 'update') view = 'model'
    if (this.state.currentNid !== props.currentNid) this.handleViewChange(props.currentNid, view)
  }

  simpleCompiler (nid, onlyOneStep) {
    if (nid === undefined || nid === null) return
    console.debug('SynthesisView:simpleCompiler(', nid, onlyOneStep, ')')
    hapi.simpleCompiler(nid, onlyOneStep)
      .then(response => {
        var newNid = response.data
        this.propagateCurrentNid(newNid)
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
              <a className='button primary' onClick={() => this.handleViewChange(this.state.currentNid, 'scOptions')}>SC options</a>
              <a className='button primary' onClick={() => this.simpleCompiler(this.state.currentNid, true)}>one step</a>
              <a className='button primary' onClick={() => this.simpleCompiler(this.state.currentNid, false)}>all steps</a>
            </div>
            { this.state.view === 'update' && <pre> updating... </pre> }
            { this.state.view === 'model' && <pre> { JSON.stringify(this.state.model, null, 2) } </pre> }
            { this.state.view === 'process' && <ProcessView
              steps={this.state.model.nitta.process.steps}
              relations={this.state.model.nitta.process.relations}
            /> }
            { this.state.view === 'scOptions' && <StepOptionView
              currentNid={this.state.currentNid}
              propagateCurrentNid={nid => this.propagateCurrentNid(nid)}
            /> }
          </div>
        }
      </div>
    )
  }
}

class StepOptionView extends Component {
  constructor (props) {
    super(props)
    this.state = {
      currentNid: props.currentNid,
      options: null
    }
    this.propagateCurrentNid = props.propagateCurrentNid
    this.updateSCOptions(props.currentNid)
  }

  componentWillReceiveProps (props) {
    console.debug('StepOptionView:componentWillReceiveProps(', props, ')')
    if (this.state.currentNid !== props.currentNid) this.updateSCOptions(props.currentNid)
    this.setState({currentNid: props.currentNid})
  }

  updateSCOptions (nid) {
    if (nid === undefined || nid === null) return
    console.debug('StepOptionView:updateSCOptions§(', nid, ')')
    hapi.simpleCompilerOptions(nid)
      .then(response => {
        this.setState({
          options: response.data
        })
      })
      .catch(err => console.log(err))
  }

  render () {
    if (this.state.options === undefined || this.state.options === null) return <div />
    if (this.state.options.length === 0) return <pre> Process is over. Options not allow. </pre>
    return (
      <div>
        <div className='grid-x'>
          <div className='cell small-4'>
            <pre>{ JSON.stringify(this.state.options[0][1], null, 2) }</pre>
          </div>
          <div className='cell small-8'>
            <LineChart data={[ this.state.options.map((e, index) => { return { x: index, y: e[0] } }) ]}
              width={750} height={250}
              axes />
          </div>
        </div>
        <ReactTable
          columns={
            [
              {
                Header: 'Integral',
                accessor: '0',
                maxWidth: 70,
                Cell: row =>
                  <a onClick={() => {
                    hapi.manualDecision(this.state.currentNid, row.index)
                      .then(response => {
                        this.propagateCurrentNid(response.data)
                      })
                      .catch(err => alert(err))
                  }}> { row.value }
                  </a>
              },
              {Header: 'Description', accessor: '3', Cell: row => <pre> {JSON.stringify(row.value)} </pre>},
              {Header: 'Metrics', accessor: '2', Cell: row => <pre> {JSON.stringify(row.value) } </pre>}
            ]
          }
          data={this.state.options}
        />
      </div>
    )
  }
}

export default App
