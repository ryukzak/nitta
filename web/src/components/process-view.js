import React, { Component } from 'react'
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
    var processUnitsAndLevels = {}
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
      e[7] = e[0] in relations ? relations[e[0]] : null // Dependencies
      if (!(step.sLevel in levels)) levels[step.sLevel] = true
      if (!(step.sPU in processUnits)) processUnits[step.sPU] = true
      var pul = step.sPU + '.' + step.sLevel
      if (!(pul in processUnitsAndLevels)) processUnitsAndLevels[pul] = true
      steps.push(e)
    }
    this.state = {
      steps: steps,
      steps_raw: props.steps,
      levels: levels,
      processUnits: processUnits,
      processUnitsAndLevels: processUnitsAndLevels
    }
  }
  changeFilter (filterName, k) {
    var filter = Object.assign({}, this.state[filterName])
    filter[k] = !filter[k]
    var updState = {}
    updState[filterName] = filter
    this.setState(updState)
  }
  filterOnly (filterName, showOnly) {
    var filter = {}
    var keys = Object.keys(this.state[filterName])
    for (var k in keys) {
      filter[keys[k]] = false
    }
    // filter[showOnly] = true
    // FIXME: not needed, because changeFilter will be fired automatically.
    var updState = {}
    updState[filterName] = filter
    this.setState(updState)
  }
  processUnitAndLevel (stepRaw) {
    return stepRaw.sPU + '.' + stepRaw.sLevel
  }
  render () {
    var steps = []
    var stepKeys = {}
    var i
    for (i = 0; i < this.state.steps.length; i++) {
      var stepRaw = this.state.steps_raw[i]
      if (this.state.levels[stepRaw.sLevel] &&
        this.state.processUnits[stepRaw.sPU] &&
        this.state.processUnitsAndLevels[this.processUnitAndLevel(stepRaw)]
      ) {
        var step = this.state.steps[i].map(e => { return e })
        steps.push(step)
        stepKeys[this.state.steps[i][0]] = true
      }
    }
    for (i in steps) {
      var deps = steps[i][7]
      if (deps == null) continue
      var newDeps = []
      for (var j in deps) {
        if (deps[j] in stepKeys) newDeps.push(deps[j])
      }
      steps[i][7] = newDeps.join()
    }
    return (
      <div>
        <h2>Filters:</h2>
        <div className='grid-x'>
          <div className='cell small-4'>
            <h4>Levels</h4>
            {
              Object.keys(this.state.levels).map(
                (k, i) => <div key={'level: ' + k}>
                  <input type='checkbox' id={k} name={k}
                    checked={this.state.levels[k]}
                    onChange={() => { this.changeFilter('levels', k) }}
                  />
                  <label htmlFor={k}>
                    {k}
                    <a onClick={() => { this.filterOnly('levels', k) }}> only </a>
                  </label>
                </div>
              )}
          </div>
          <div className='cell small-4'>
            <h4>Process units</h4>
            {Object.keys(this.state.processUnits).map(
              (k, i) => <div key={'processUnit: ' + k}>
                <input type='checkbox' id={k} name={k}
                  checked={this.state.processUnits[k]}
                  onChange={() => { this.changeFilter('processUnits', k) }}
                />
                <label htmlFor={k}>
                  {k}
                  <a onClick={() => { this.filterOnly('processUnits', k) }}> only </a>
                </label>
              </div>
            )}
          </div>
          <div className='cell small-4'>
            <h4>ProcessUnits and Levels</h4>
            {
              Object.keys(this.state.processUnitsAndLevels).map(
                (k, i) => <div key={'processUnitAndLevel: ' + k}>
                  <input type='checkbox' id={k} name={k}
                    checked={this.state.processUnitsAndLevels[k]}
                    onChange={() => { this.changeFilter('processUnitsAndLevels', k) }}
                  />
                  <label htmlFor={k}>
                    {k}
                    <a onClick={() => { this.filterOnly('processUnitsAndLevels', k) }}> only </a>
                  </label>
                </div>
              )}
          </div>
        </div>
        <hr />
        { steps.length === 0
          ? (<pre> Process is empty </pre>)
          : <Chart
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
        }
      </div>
    )
  }
}
