import React, { Component } from 'react'
import 'react-table/react-table.css'
import { ProcessView } from './process-view'
import { hapi } from '../hapi'
import { EdgesView } from './edges-view'

export class NodeView extends Component {
  constructor (props) {
    super(props)
    this.onNIdChange = props.onNIdChange
    this.state = {
      selectedNId: props.selectedNId,
      synthesisStatus: props.synthesisStatus,
      view: 'update',
      model: null,
      scOptions: null
    }
    this.handleViewChange(props.selectedNId, props.synthesisStatus, 'synthesisNode')
  }

  handleViewChange (nid, synthesisStatus, view) {
    console.debug('NodeView:handleViewChange(', nid, view, ') // this.state.view:', this.state.view)
    if (nid === undefined || nid === null) return

    this.setState({selectedNId: nid, synthesisStatus: synthesisStatus, view: 'update'})
    if (view === 'process') this.updateModel(nid, 'process')
    if (view === 'synthesisNode') this.updateModel(nid, 'synthesisNode')
    if (view === 'testbench') this.updateTestBench(nid, 'testbench')
    if (view === 'edges') this.setState({view: 'edges'})
  }

  componentWillReceiveProps (props) {
    console.debug('NodeView:componentWillReceiveProps(', props, ')')
    var view = this.state.view
    if (view === 'update') view = 'synthesisNode'
    if (this.state.selectedNId !== props.selectedNId) {
      this.handleViewChange(props.selectedNId, props.synthesisStatus, view)
    }
  }

  allBestThread (nid, n) {
    if (nid === undefined || nid === null) return
    console.debug('NodeView:allBestThread(', nid, n, ')')
    hapi.allBestThread(nid, n)
      .then(response => {
        var newNid = response.data
        this.onNIdChange(newNid)
      })
      .catch(err => alert(err))
  }

  obviousBindThread (nid) {
    if (nid === undefined || nid === null) return
    console.debug('NodeView:obviousBindThread(', nid, ')')
    hapi.obviousBindThread(nid)
      .then(response => {
        var newNid = response.data
        this.onNIdChange(newNid)
      })
      .catch(err => alert(err))
  }

  simpleSynthesis (nid, deep) {
    if (nid === undefined || nid === null) return
    console.debug('NodeView:simpleSynthesis(', nid, ')')
    hapi.simpleSynthesis(nid, deep)
      .then(response => {
        var newNid = response.data
        this.onNIdChange(newNid)
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
        this.handleViewChange(this.state.selectedNId, this.state.synthesisStatus, 'synthesisNode')
      })
  }

  render (props) {
    return (
      <div>
        { this.state.selectedNId === null && <pre> synthesis is not selected </pre> }

        { this.state.selectedNId !== null &&
          <div>
            <div className='tiny button-group'>
              <a className='button primary' onClick={() => this.handleViewChange(this.state.selectedNId, this.state.synthesisStatus, 'synthesisNode')}>synthesis node</a>
              <a className='button primary' onClick={() => this.handleViewChange(this.state.selectedNId, this.state.synthesisStatus, 'process')}>process</a>
              <a className='button primary' onClick={() => this.handleViewChange(this.state.selectedNId, this.state.synthesisStatus, 'edges')}>edges</a>
              <a className='button primary' onClick={() => this.handleViewChange(this.state.selectedNId, this.state.synthesisStatus, 'testbench')}>testbench</a>
            </div>
            <div className='tiny button-group'>
              <a className='button primary' onClick={() => this.simpleSynthesis(this.state.selectedNId)}>simple synthesis</a>
            </div>
            <div className='tiny button-group'>
              <a className='button primary' onClick={() => this.obviousBindThread(this.state.selectedNId)}>oblious bind thread</a>
              <a className='button primary' onClick={() => this.allBestThread(this.state.selectedNId, 0)}>best thread</a>
              <a className='button primary' onClick={() => this.allBestThread(this.state.selectedNId, 1)}>all best thread 1</a>
              <a className='button primary' onClick={() => this.allBestThread(this.state.selectedNId, 2)}>all best thread 2</a>
            </div>
            { this.state.view === 'update' && <pre> updating... </pre> }
            { this.state.view === 'synthesisNode' &&
              <div className='grid-x'>
                <div className='cell small-4'>
                  <small><pre> processor: </pre></small>
                  <small><pre> { JSON.stringify(this.state.synthesisNode.nModel.processor, null, 2) } </pre></small>
                </div>
                <div className='cell small-4'>
                  <small><pre> data flow graph: </pre></small>
                  <small><pre> { JSON.stringify(this.state.synthesisNode.nModel.dfg, null, 2) } </pre></small>
                </div>
                <div className='cell small-4'>
                  <small><pre> nId: { JSON.stringify(this.state.synthesisNode.nId, null, 2) } </pre></small>
                  <small><pre> nIsComplete: { JSON.stringify(this.state.synthesisNode.nIsComplete, null, 2) } </pre></small>
                </div>
              </div> }
            { this.state.view === 'process' && <ProcessView
              steps={this.state.synthesisNode.nModel.processor.process.steps}
              relations={this.state.synthesisNode.nModel.processor.process.relations}
            /> }
            { this.state.view === 'edges' && <EdgesView
              selectedNId={this.state.selectedNId}
              onNIdChange={nid => this.onNIdChange(nid)}
            /> }
            { this.state.view === 'testbench' &&
              <div>
                Status: <pre> { JSON.stringify(this.state.testBenchDump.tbStatus) } </pre>
                <hr />
                Compiler output:
                <pre> { this.state.testBenchDump.tbCompilerDump } </pre>
                <hr />
                Simulation output:
                <pre> { this.state.testBenchDump.tbSimulationDump } </pre>
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
