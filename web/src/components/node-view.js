import React, { Component } from 'react'
import 'react-table/react-table.css'
import { ProcessView } from './process-view'
import { hapi } from '../hapi'
import { EdgesView } from './edges-view'

export class NodeView extends Component {
  constructor (props) {
    super(props)
    this.onCurrentNIdChange = props.onCurrentNIdChange
    this.state = {
      currentNId: props.currentNId,
      synthesisStatus: props.synthesisStatus,
      view: 'update',
      model: null,
      scOptions: null
    }
    this.handleViewChange(props.currentNId, props.synthesisStatus, 'synthesisNode')
  }

  handleViewChange (nid, synthesisStatus, view) {
    console.debug('NodeView:handleViewChange(', nid, view, ') // this.state.view:', this.state.view)
    if (nid === undefined || nid === null) return

    this.setState({currentNId: nid, synthesisStatus: synthesisStatus, view: 'update'})
    if (view === 'process') this.updateModel(nid, 'process')
    if (view === 'synthesisNode') this.updateModel(nid, 'synthesisNode')
    if (view === 'testbench') this.updateTestBench(nid, 'testbench')
    if (view === 'edges') this.setState({view: 'edges'})
  }

  componentWillReceiveProps (props) {
    console.debug('NodeView:componentWillReceiveProps(', props, ')')
    var view = this.state.view
    if (view === 'update') view = 'synthesisNode'
    if (this.state.currentNId !== props.currentNId) {
      this.handleViewChange(props.currentNId, props.synthesisStatus, view)
    }
  }

  allBestThread (nid, n) {
    if (nid === undefined || nid === null) return
    console.debug('NodeView:allBestThread(', nid, n, ')')
    hapi.allBestThread(nid, n)
      .then(response => {
        var newNid = response.data
        this.onCurrentNIdChange(newNid)
      })
      .catch(err => alert(err))
  }

  obviousBindThread (nid) {
    if (nid === undefined || nid === null) return
    console.debug('NodeView:obviousBindThread(', nid, ')')
    hapi.obviousBindThread(nid)
      .then(response => {
        var newNid = response.data
        this.onCurrentNIdChange(newNid)
      })
      .catch(err => alert(err))
  }

  simpleSynthesis (nid, deep) {
    if (nid === undefined || nid === null) return
    console.debug('NodeView:simpleSynthesis(', nid, ')')
    hapi.simpleSynthesis(nid, deep)
      .then(response => {
        var newNid = response.data
        this.onCurrentNIdChange(newNid)
      })
      .catch(err => alert(err))
  }

  updateModel (nid, view) {
    hapi.getNode(nid)
      .then(response => {
        this.setState({
          synthesisNode: response.data,
          view: view
        })
      })
      .catch(err => console.log(err))
  }

  updateTestBench (nid, view) {
    hapi.runTestBench(nid, 'web_ui')
      .then(response => {
        this.setState({
          testBenchDump: response.data,
          view: view
        })
      })
      .catch(err => {
        alert('Can not take testbench, maybe, because it is not completed synthesis!\n' + err)
        this.handleViewChange(this.state.currentNId, this.state.synthesisStatus, 'synthesisNode')
      })
  }

  render (props) {
    return (
      <div>
        { this.state.currentNId === null && <pre> synthesis is not selected </pre> }

        { this.state.currentNId !== null &&
          <div>
            <div className='tiny button-group'>
              <a className='button primary' onClick={() => this.handleViewChange(this.state.currentNId, this.state.synthesisStatus, 'synthesisNode')}>synthesis node</a>
              <a className='button primary' onClick={() => this.handleViewChange(this.state.currentNId, this.state.synthesisStatus, 'process')}>process</a>
              <a className='button primary' onClick={() => this.handleViewChange(this.state.currentNId, this.state.synthesisStatus, 'edges')}>edges</a>
              <a className='button primary' onClick={() => this.handleViewChange(this.state.currentNId, this.state.synthesisStatus, 'testbench')}>testbench</a>
            </div>
            <div className='tiny button-group'>
              <a className='button primary' onClick={() => this.simpleSynthesis(this.state.currentNId)}>simple synthesis</a>
            </div>
            <div className='tiny button-group'>
              <a className='button primary' onClick={() => this.obviousBindThread(this.state.currentNId)}>oblious bind thread</a>
              <a className='button primary' onClick={() => this.allBestThread(this.state.currentNId, 0)}>best thread</a>
              <a className='button primary' onClick={() => this.allBestThread(this.state.currentNId, 1)}>all best thread 1</a>
              <a className='button primary' onClick={() => this.allBestThread(this.state.currentNId, 2)}>all best thread 2</a>
            </div>
            { this.state.view === 'update' && <pre> updating... </pre> }
            { this.state.view === 'synthesisNode' && <pre> { JSON.stringify(this.state.synthesisNode, null, 2) } </pre> }
            { this.state.view === 'process' && <ProcessView
              steps={this.state.synthesisNode.nModel.processor.process.steps}
              relations={this.state.synthesisNode.nModel.processor.process.relations}
            /> }
            { this.state.view === 'edges' && <EdgesView
              currentNId={this.state.currentNId}
              onCurrentNIdChange={nid => this.onCurrentNIdChange(nid)}
            /> }
            { this.state.view === 'testbench' && 
              <div>
                Status: <pre> { JSON.stringify(this.state.testBenchDump.tbStatus) } </pre>
                Stdout:
                <hr />
                <pre> { this.state.testBenchDump.tbSimulationStdout } </pre>
                <hr />
                Stderr:
                <hr />
                <pre> { this.state.testBenchDump.tbSimulationErrout } </pre>
                <hr />
                <pre> { JSON.stringify(this.state.testBenchDump, null, 2) } </pre>
              </div>
            }
          </div>
        }
      </div>
    )
  }
}
