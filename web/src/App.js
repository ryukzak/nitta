import React, { Component } from 'react'
import './App.css'
import api from './gen/nitta-api.js'
import ReactTable from 'react-table'
import 'react-table/react-table.css'
import { LineChart } from 'react-easy-chart'
import { ProcessView } from './process-view.js'

var hapi = {}
hapi.forkSynthesis = api.postSynthesisBySid
hapi.manualDecision = api.postSynthesisBySidSStepsBySix
hapi.getStep = api.getSynthesisBySidSStepsBySix
hapi.getStepOption = api.getSynthesisBySidSStepsBySixOptions
hapi.compilerStep = api.postSynthesisBySidSSteps
hapi.getSynthesis = function (sid) {
  if (sid === undefined) {
    return api.getSynthesis()
  } else {
    return api.getSynthesisBySid(sid)
  }
}

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
    hapi.getSynthesis()
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
    hapi.getSynthesis(sId)
      .then(response => {
        this.setState({
          path: this.root(sId, 'info'),
          sData: response.data
        })
      })
      .catch(err => console.log(err))
  }

  forkSynthesis (path, props) {
    var sId = path.sId + '.' + path.stepId
    while (props.app.state.synthesisList.indexOf(sId) >= 0) {
      sId += "'"
    }
    hapi.forkSynthesis(sId, path.sId, path.stepId)
    props.app.getAllSynthesis()
    props.app.getSynthesis(sId)
  }

  getStep (sId, stepId) {
    hapi.getStep(sId, stepId)
      .then(response => {
        this.setState({
          path: this.root(sId, 'step', stepId),
          stepData: response.data
        })
        this.getStepOption(sId, stepId)
      })
      .catch(err => console.log(err))
  }

  getStepOption (sId, stepId) {
    hapi.getStepOption(sId, stepId)
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
            { sData.sSteps.map((st, i) =>
              <LinkButton key={i} sname={i}
                onClick={() => { props.app.getStep(path.sId, i) }}
              />
            )}
            <a className='button primary' onClick={
              () => {
                hapi.compilerStep(path.sId, false)
                  .then(response => {
                    props.app.getSynthesis(response.data[0])
                    props.app.getStep(response.data[0], response.data[1])
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
      <dd> { sData.sParent
        ? (<LinkButton sname={sData.sParent[0]} onClick={() => app.getSynthesis(sData.sParent[0])} />)
        : (<div> - </div>) }
      </dd>
      <dt>Childs:</dt>
      <dd>
        <div className='tiny button-group'>
          { sData.sChilds.map((k, i) => <LinkButton key={i} sname={k[0]} onClick={() => app.getSynthesis(k[0])} />) }
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
  return (
    <div>
      <div className='tiny primary button-group'>
        <LinkButton sname='options' onClick={
          () => app.getStepOption(path.sId, path.stepId)} />
        <LinkButton sname='info' onClick={
          () => app.setPath(path.sId, 'step', path.stepId, 'info')} />
        <LinkButton sname='process' onClick={
          () => app.setPath(path.sId, 'step', path.stepId, 'process')} />
        <LinkButton sname='fork' onClick={() => { this.forkSynthesis(path, props) }} />
        <LinkButton sname='mk_decision' onClick={
          () => {
            hapi.compilerStep(path.sId, true)
              .then(response => {
                props.app.getSynthesis(response.data[0])
                props.app.getStep(response.data[0], response.data[1])
              })
              .catch(err => alert(err))
          }} />
      </div>
      <pre>{ JSON.stringify(step.lastDecision) }</pre>
      <hr />
      { (path.stepView === 'options')
        ? <StepOptionView path={path} app={app} stepDataOptions={app.state.stepDataOptions} />
        : (path.stepView === 'process')
          ? <ProcessView
            steps={app.state.stepData.state.nitta.process.steps}
            relations={app.state.stepData.state.nitta.process.relations}
          />
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
      <div className='grid-x'>
        <div className='cell small-4'>
          <pre>{ JSON.stringify(opts[0][1], null, 2) }</pre>
        </div>
        <div className='cell small-8'>
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
                    hapi.manualDecision(path.sId, path.stepId + 1, row.index)
                      .then(response => {
                        // FIXME: Не обновляется список синтезов.
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
