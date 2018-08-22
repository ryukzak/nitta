import React, { Component } from 'react'
import './App.css'
import api from './gen/nitta-api.js'
import ReactTable from 'react-table'
import 'react-table/react-table.css'
import { LineChart } from 'react-easy-chart'
import { Chart } from 'react-google-charts'

class App extends Component {
  constructor () {
    super()
    this.state = {
      path: this.root(),
      synthesisList: [],
      sData: null,
      stepData: null,
      stepOption: null
    }
    this.getAllSynthesis = this.getAllSynthesis.bind(this)
    this.path = this.path.bind(this)
    this.setPath = this.setPath.bind(this)
    this.getAllSynthesis()
  }

  getAllSynthesis () {
    api.getSynthesis()
      .then(response => this.setState({
        path: this.root(),
        synthesisList: Object.keys(response.data),
        sData: null,
        stepData: null
      }))
      .catch(err => console.log(err))
  }

  getSynthesis (sId) {
    // TODO: Смена синтеза без смены шина. Необходимо для простоты сравнения вариантов винтеза.
    api.getSynthesisBySId(sId)
      .then(response => {
        this.setState({
          path: this.root(sId, 'info'),
          sData: response.data
        })
      })
      .catch(err => console.log(err))
  }

  getStep (sId, stepId) {
    api.getSynthesisBySIdStepsByStepId(sId, stepId)
      .then(response => {
        var steps = []
        var levels = {}
        var processUnits = {}
        for (var k in response.data.state.nitta.process.steps) {
          var step = response.data.state.nitta.process.steps[k]
          var e = [ null, null, null, null, null, null, null ]
          e[0] = step.sKey.toString() // Task ID
          e[1] = step.sDesc // Task Name
          // Resource ID (optional)
          e[2] = new Date(step.sTime[0] * 1000) // Start
          e[3] = null // End
          e[4] = step.sTime[1] != null ? (step.sTime[1] + 1) * 1000 : 1000000 // Duration
          e[5] = 100 // Percent Complete
          e[6] = null // Dependencies
          if (!(step.sLevel in levels)) levels[step.sLevel] = true
          if (!(step.sPU in processUnits)) processUnits[step.sPU] = true
          steps.push(e)
        }
        response.data.state.nitta.process.steps_raw = response.data.state.nitta.process.steps
        response.data.state.nitta.process.steps = steps
        response.data.state.nitta.process.levels = levels
        response.data.state.nitta.process.processUnits = processUnits
        this.setState({
          path: this.root(sId, 'step', stepId),
          stepData: response.data,
          _p: response.data.state.nitta.process
        })
        this.getStepOption(sId, stepId)
      })
      .catch(err => console.log(err))
  }

  getStepOption (sId, stepId) {
    api.getSynthesisBySIdStepsByStepIdOptions(sId, stepId)
      .then(response => this.setState({
        path: this.root(sId, 'step', stepId, 'options'),
        stepDataOptions: response.data
      }))
      .catch(err => console.log(err))
  }

  root (sId, sView, stepId, stepView) {
    return {
      sId: sId,
      sView: sView,
      stepId: stepId,
      stepView: stepView
    }
  }

  setPath (sId, sView, stepId, stepView) {
    console.log(this.root(sId, sView, stepId))
    this.setState({
      path: this.root(sId, sView, stepId, stepView)
    })
  }

  path () {
    var result = []
    if (this.state.path.sId) { result.push(this.state.path.sId) }
    if (this.state.path.sView) { result.push(this.state.path.sView) }
    if (this.state.path.stepId) { result.push(this.state.path.stepId) }
    if (this.state.path.stepView) { result.push(this.state.path.stepView) }
    return result
  }

  render () {
    return (
      <div>
        <nav aria-label='You are here:'>
          <ul className='breadcrumbs'>
            <li>Project</li>
            { this.path().map((v, i) => <li key={i}> { v } </li>) }
          </ul>
        </nav>

        <div className='tiny button-group'>
          <a className='button primary' onClick={this.getAllSynthesis}>Refresh</a>
          { this.state.synthesisList.map((sname, i) =>
            <LinkButton key={i} sname={sname} onClick={() => this.getSynthesis(sname)} />) }
        </div>

        <View path={this.state.path} sData={this.state.sData} stepData={this.state.stepData} app={this} />
      </div>
    )
  }
}

function View (props) {
  var path = props.path
  var sData = props.sData
  return (
    <div>
      { path.sId
        ? <div>
          <div className='tiny button-group'>
            <a className='button primary' onClick={() => props.app.setPath(path.sId, 'info')}>Info</a>
            { sData.steps.map((st, i) =>
              <LinkButton key={i} sname={i}
                onClick={() => { props.app.getStep(path.sId, i) }}
              />
            )}
            <a className='button primary' onClick={
              () => {
                api.postSynthesisBySIdSteps(path.sId, true)
                  .then(response => {
                    props.app.getSynthesis(path.sId)
                    props.app.getStep(path.sId, 0)
                  })
                  .catch(err => alert(err))
              }
            }>auto_synthesis</a>
          </div>
          { path.sView === 'info'
            ? <SynthesisInfo sData={sData} app={props.app} />
            : path.sView === 'step'
              ? <StepView path={path} stepData={props.stepData} app={props.app} />
              : <pre>path.sView = {path.sView}</pre>
          }
        </div>
        : <pre> SYNTHESIS NOT SELECTED </pre>
      }
    </div>
  )
}

function SynthesisInfo (props) {
  var sData = props.sData
  var app = props.app
  return (
    <dl>
      <dt>Parent:</dt>
      <dd> { sData.parent
        ? (<LinkButton sname={sData.parent[0]} onClick={() => app.getSynthesis(sData.parent[0])} />)
        : (<div> - </div>) }
      </dd>
      <dt>Childs:</dt>
      <dd>
        <div className='tiny button-group'>
          { sData.childs.map((k, i) => <LinkButton key={i} sname={k[0]} onClick={() => app.getSynthesis(k[0])} />) }
        </div>
      </dd>
      <dt>Config (may vary from step to step):</dt>
      <dd>
        <pre>{ JSON.stringify(sData.config, null, 2) }</pre>
      </dd>
    </dl>
  )
}

function StepView (props) {
  var path = props.path
  var app = props.app
  var step = props.app.state.stepData
  var levels = app.state.stepData.state.nitta.process.levels
  var processUnits = app.state.stepData.state.nitta.process.processUnits
  var steps = []
  var steps_raw = app.state.stepData.state.nitta.process.steps_raw
  var steps_to_view = app.state.stepData.state.nitta.process.steps

  for (var i = 0; i < steps_raw.length; i++) {
    console.log(steps_raw[i].sLevel, levels[steps_raw[i].sLevel], steps_raw[i].sPU, processUnits[steps_raw[i].sPU])
    if (levels[steps_raw[i].sLevel] && processUnits[steps_raw[i].sPU]) steps.push(steps_to_view[i])
  }
  return (
    <div>
      <div className='tiny primary button-group'>
        <LinkButton sname='options' onClick={
          () => app.getStepOption(path.sId, path.stepId)} />
        <LinkButton sname='info' onClick={
          () => app.setPath(path.sId, 'step', path.stepId, 'info')} />
        <LinkButton sname='process' onClick={
          () => app.setPath(path.sId, 'step', path.stepId, 'process')} />
        <LinkButton sname='fork' onClick={
          () => {
            var sId = path.sId + '.' + path.stepId
            while (props.app.state.synthesisList.indexOf(sId) >= 0) {
              sId += "'"
            }
            api.postSynthesisBySId(sId, path.sId, path.stepId)
            props.app.getAllSynthesis()
            props.app.getSynthesis(sId)
          } } />
        <LinkButton sname='mk_decision' onClick={
          () => {
            api.postSynthesisBySIdSteps(path.sId, false)
              .then(response => {
                props.app.getSynthesis(path.sId)
                props.app.getStep(path.sId, path.stepId + 1)
              })
              .catch(err => alert(err))
          }} />
      </div>
      <pre>{ JSON.stringify(step.lastDecision) }</pre>
      <hr />
      { (path.stepView === 'options')
        ? <StepOptionView path={path} app={app} stepDataOptions={app.state.stepDataOptions} />
        : (path.stepView === 'process')
          ? <div>
            <div class='grid-x'>
              <div class='cell small-6'>
                <h4>Levels</h4>
                { Object.keys(levels).map(
                  (k, i) => <div>
                    <input type='checkbox' id={k} name={k}
                      defaultChecked={levels[k]}
                      onChange={function () { levels[k] = !levels[k] }}
                    />
                    <label for={k}> { k} </label>
                  </div>
                )}
              </div>
              <div class='cell small-6'>
                <h4>Process units</h4>
                { Object.keys(processUnits).map(
                  (k, i) => <div>
                    <input type='checkbox' id={k} name={k}
                      defaultChecked={processUnits[k]}
                      onChange={function () { processUnits[k] = !processUnits[k] }}
                    />
                    <label for={k}> { k} </label>
                  </div>
                )}
              </div>
            </div>
            <hr />
            <pre>FIXME: For refresh Gantt charts - change the view and jump back.</pre>
            <Chart
              chartType='Gantt'
              columns={[
                {'id': 'Task ID', 'type': 'string'},
                {'id': 'Task Name', 'type': 'string'},
                {'id': 'Start Date', 'type': 'date'},
                {'id': 'End Date', 'type': 'date'},
                {'id': 'Duration', 'type': 'number'},
                {'id': 'Percent Complete', 'type': 'number'},
                {'id': 'Dependencies', 'type': 'string'}
              ]}
              rows={steps}
              width='100%'
              height={(steps.length + 1) * 21 + 30}
              options={{
                gantt: {
                  trackHeight: 20,
                  barHeight: 10
                }}}
            />
          </div>
          : (path.stepView === 'info')
            ? <div>
              <pre>
                { JSON.stringify(app.state.stepData, null, 2) }
              </pre>
            </div>
            : <pre>path.stepView = { path.stepView }</pre>
      }
    </div>
  )
}

function StepOptionView (props) {
  var path = props.path
  var opts = props.stepDataOptions
  if (opts.length === 0) return <pre> Process is over. Options not allow. </pre>
  return (
    <div>
      <div class='grid-x'>
        <div class='cell small-4'>
          <pre>{ JSON.stringify(opts[0][1], null, 2) }</pre>
        </div>
        <div class='cell small-8'>
          <LineChart data={[ opts.map((e, index) => { return { x: index, y: e[0] } }) ]}
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
                <a onClick={
                  () => {
                    api.postSynthesisBySIdStepsByStepId(path.sId, path.stepId + 1, row.index)
                      .then(response => {
                        props.app.getSynthesis(path.sId)
                        props.app.getStep(path.sId, path.stepId + 1)
                      })
                      .catch(err => alert(err))
                  }}> { row.value }
                </a>
            },
            {Header: 'Description', accessor: '3', Cell: row => <pre> {JSON.stringify(row.value)} </pre>},
            {Header: 'Metrics', accessor: '2', Cell: row => <pre> {JSON.stringify(row.value) } </pre>}
          ]
        }
        data={opts}
      />
    </div>
  )
}

function LinkButton (props) {
  return (
    <a className='button tiny secondary' onClick={props.onClick}> {props.sname} </a>
  )
}

export default App
