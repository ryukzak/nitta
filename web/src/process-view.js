import React, { Component } from 'react'
import './App.css'
import 'react-table/react-table.css'
import { Chart } from 'react-google-charts'

export class ProcessView extends Component {
  constructor (props) {
    super(props)
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
      e[4] = null // End
      e[5] = step.sTime[1] != null ? (step.sTime[1] + 1) * 1000 : 1000000 // Duration
      e[6] = 100 // Percent Complete
      e[7] = null // Dependencies
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
    console.log(steps)
    for (var i = 0; i < this.state.steps.length; i++) {
      console.log(this.state.steps_raw[i].sLevel, this.state.levels[this.state.steps_raw[i].sLevel], this.state.steps_raw[i].sPU, this.state.processUnits[this.state.steps_raw[i].sPU])
      if (this.state.levels[this.state.steps_raw[i].sLevel] && this.state.processUnits[this.state.steps_raw[i].sPU]) {
        steps.push(this.state.steps[i])
      }
    }
    if (steps.length === 0) return (<pre> Process is empty </pre>)

    return (
      <div>
        <div class='grid-x'>
          <div class='cell small-6'>
            <h4>Levels</h4>
            {
              Object.keys(this.state.levels).map(
                (k, i) => <div>
                  <input type='checkbox' id={k} name={k} key={k}
                    defaultChecked={this.state.levels[k]}
                    onChange={() => { this.changeLevels(k) } }
                  />
                  <label for={k}> {k} </label>
                </div>
              )}
          </div>
          <div class='cell small-6'>
            <h4>Process units</h4>
            {Object.keys(this.state.processUnits).map(
              (k, i) => <div>
                <input type='checkbox' id={k} name={k} key={k}
                  defaultChecked={this.state.processUnits[k]}
                  onChange={() => { this.changeProcessUnits(k) }}
                />
                <label for={k}> {k} </label>
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
          height={(steps.length + 1) * 21 + 30}
          options={{
            gantt: {
              trackHeight: 20,
              barHeight: 10
            }
          }}
        />
      </div>
    )
  }
}
