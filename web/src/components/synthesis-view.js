import React, { Component } from 'react'
import 'react-table/react-table.css'
import { ProcessView } from './process-view'
import { hapi } from '../hapi'
import { SimpleCompilerView } from './compiler-view'

export class SynthesisView extends Component {
  constructor (props) {
    super(props)
    this.onCurrentNidChange = props.onCurrentNidChange
    this.state = {
      currentNid: props.currentNid,
      view: 'update',
      model: null,
      scOptions: null
    }
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
        this.onCurrentNidChange(newNid)
      })
      .catch(err => alert(err))
  }

  obviousBind (nid) {
    if (nid === undefined || nid === null) return
    console.debug('SynthesisView:obviousBind(', nid, ')')
    hapi.obviousBind(nid)
      .then(response => {
        var newNid = response.data
        this.onCurrentNidChange(newNid)
      })
      .catch(err => alert(err))
  }

  simpleAllThreads (nid, deep) {
    if (nid === undefined || nid === null) return
    console.debug('SynthesisView:obviousBind(', nid, ')')
    hapi.simpleAllThreads(nid, deep)
      .then(response => {
        var newNid = response.data
        this.onCurrentNidChange(newNid)
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
              <a className='button primary' onClick={() => this.simpleCompiler(this.state.currentNid, true)}>step</a>
              <a className='button primary' onClick={() => this.simpleCompiler(this.state.currentNid, false)}>thread</a>
              <a className='button primary' onClick={() => this.obviousBind(this.state.currentNid)}>obvious bind;</a>
              <a className='button primary' onClick={() => this.simpleAllThreads(this.state.currentNid, 1)}>all threads #1</a>
              <a className='button primary' onClick={() => this.simpleAllThreads(this.state.currentNid, 2)}>#2</a>
              <a className='button primary' onClick={() => this.simpleAllThreads(this.state.currentNid, 3)}>#3</a>
            </div>
            { this.state.view === 'update' && <pre> updating... </pre> }
            { this.state.view === 'model' && <pre> { JSON.stringify(this.state.model, null, 2) } </pre> }
            { this.state.view === 'process' && <ProcessView
              steps={this.state.model.nitta.process.steps}
              relations={this.state.model.nitta.process.relations}
            /> }
            { this.state.view === 'scOptions' && <SimpleCompilerView
              currentNid={this.state.currentNid}
              onCurrentNidChange={nid => this.onCurrentNidChange(nid)}
            /> }
          </div>
        }
      </div>
    )
  }
}
