import React, { Component } from 'react'
import './App.css'
import 'react-table/react-table.css'
import { Chart } from 'react-google-charts'

export class ProcessView extends Component {
  constructor (props) {
    super(props)
    var relations = {}
    for (var r in props.relations) {
      var a = props.relations[r][1].toString()
      var b = props.relations[r][0].toString()
      if (!(a in relations)) relations[a] = []
      relations[a].push(b)
    }
    var steps = []
    var levels = {}
    var processUnits = {}
    for (var k in props.steps) {
      var step = props.steps[k]
      var e = [null, null, null, null, null, null, null, null]
      e[0] = step.sKey.toString() // Task ID
      e[1] = step.sDesc // Task Name
      e[2] = step.sPU // Resource ID (optional)
      e[3] = new Date(step.sTime[0] * 1000) // Start
      e[4] = new Date(step.sTime[1] != null ? (step.sTime[1] + 1) * 1000 : 1000000) // End
      e[5] = null // Duration
      e[6] = 100 // Percent Complete
      e[7] = e[0] in relations ? relations[e[0]].join() : null // Dependencies
      if (!(step.sLevel in levels)) levels[step.sLevel] = true
      if (!(step.sPU in processUnits)) processUnits[step.sPU] = true
      steps.push(e)
    }
    this.state = {
      steps: steps,
      steps_raw: props.steps,
      levels: levels,
      processUnits: processUnits
    }
  }
  changeLevels (l) {
    var levels = Object.assign({}, this.state.levels)
    levels[l] = !levels[l]
    this.setState({
      levels: levels
    })
  }
  changeProcessUnits (pu) {
    var processUnits = Object.assign({}, this.state.processUnits)
    processUnits[pu] = !processUnits[pu]
    this.setState({
      processUnits: processUnits
    })
  }
  render () {
    var steps = []
    for (var i = 0; i < this.state.steps.length; i++) {
      if (this.state.levels[this.state.steps_raw[i].sLevel] && this.state.processUnits[this.state.steps_raw[i].sPU]) {
        steps.push(this.state.steps[i])
      }
    }
    if (steps.length === 0) return (<pre> Process is empty </pre>)

    return (
      <div>
        <div className='grid-x'>
          <div className='cell small-6'>
            <h4>Levels</h4>
            {
              Object.keys(this.state.levels).map(
                (k, i) => <div key={'level: ' + k}>
                  <input type='checkbox' id={k} name={k}
                    defaultChecked={this.state.levels[k]}
                    onChange={() => { this.changeLevels(k) }}
                  />
                  <label htmlFor={k}> {k} </label>
                </div>
              )}
          </div>
          <div className='cell small-6'>
            <h4>Process units</h4>
            {Object.keys(this.state.processUnits).map(
              (k, i) => <div key={'processUnit: ' + k}>
                <input type='checkbox' id={k} name={k}
                  defaultChecked={this.state.processUnits[k]}
                  onChange={() => { this.changeProcessUnits(k) }}
                />
                <label htmlFor={k}> {k} </label>
              </div>
            )}
          </div>
        </div>
        <hr />
        <Chart
          chartType='Gantt'
          columns={[
            { 'id': 'Task ID', 'type': 'string' },
            { 'id': 'Task Name', 'type': 'string' },
            { 'id': 'Resource', 'type': 'string' },
            { 'id': 'Start Date', 'type': 'date' },
            { 'id': 'End Date', 'type': 'date' },
            { 'id': 'Duration', 'type': 'number' },
            { 'id': 'Percent Complete', 'type': 'number' },
            { 'id': 'Dependencies', 'type': 'string' }
          ]}
          rows={steps}
          width='100%'
          height={(steps.length + 1) * 31 + 30}
          options={{
            gantt: {
              trackHeight: 30,
              barHeight: 10,
              criticalPathEnabled: false,
              barCornerRadius: 1,
              arrow: {
                length: 4,
                radius: 5
              }
            }
          }}
        />
      </div>
    )
  }
}
