import React, { Component } from 'react'
import ReactTable from 'react-table'
import 'react-table/react-table.css'
import { LineChart } from 'react-easy-chart'
import { ProcessView } from './process-view'
import { hapi } from '../hapi'
import { LinkButton } from '../utils'

export class StepView extends Component {
  constructor (props) {
    super(props)
    this.state = {
      sRoot: props.sRoot,
      six: props.six,
      view: 'info',
      data: null,
      options: []
    }
    this.propagateSRoot = props.propagateSRoot
    this.handleSixChange(props.sRoot, props.six, true)
  }

  componentWillReceiveProps (props) {
    console.debug('StepView:componentWillReceiveProps()')
    if (this.state.sRoot !== props.sRoot ||
      this.state.six !== props.six) this.handleSixChange(props.sRoot, props.six)
  }

  handleSixChange (sRoot, six, firstTime) {
    console.debug('StepView:handleSixChange(', sRoot, six, ') // state.sRoot, state.six:', this.state.sRoot, this.state.six)
    if (firstTime ||
      (
        sRoot !== null && six !== null && sRoot !== undefined && six !== undefined &&
        (sRoot !== this.state.sRoot || six !== this.state.six)
      )
    ) {
      if (!firstTime) {
        this.setState({
          sRoot: sRoot,
          six: six,
          data: null,
          options: []
        })
      }
      hapi.getStep(sRoot, six)
        .then(response => {
          this.setState({
            data: response.data,
            options: []
          })
        })
        .catch(err => console.log(err))
    }
  }

  forkSynthesis (sRoot, six) {
    console.debug('StepView:forkSynthesis(', sRoot, six, ')')
    if (sRoot.six > six) {
      alert('if six is from previous synthesis, fork must be start early.')
      return
    }
    // FIXME: if six is from previous synthesis, fork must be start early.
    hapi.forkSynthesis(sRoot, six)
      .then(response => {
        this.propagateSRoot(response.data[0], response.data[1])
      })
      .catch(err => console.log(err))
  }

  getStepOption () {
    hapi.getStepOption(this.state.sRoot, this.state.six)
      .then(response => {
        console.debug(response)
        this.setState({
          view: 'options',
          options: response.data
        })
      })
      .catch(err => console.log(err))
  }

  render () {
    return (
      <div>
        <div className='tiny primary button-group'>
          <LinkButton sname='info' onClick={() => this.setState({view: 'info'})} />
          <LinkButton sname='options' onClick={() => this.getStepOption()} />
          <LinkButton sname='process' onClick={() => this.setState({view: 'process'})} />
          <LinkButton sname='fork' onClick={() => { this.forkSynthesis(this.state.sRoot, this.state.six) }} />
        </div>
        <hr />
        { this.state.view === 'info' && <pre>{ JSON.stringify(this.state.data, null, 2) }</pre> }
        { this.state.view === 'options' && <StepOptionView
          options={this.state.options}
          sRoot={this.state.sRoot}
          six={this.state.six}
          propagateSRoot={sRoot => this.propagateSRoot(sRoot)}
        /> }
        { this.state.view === 'process' && <ProcessView
          steps={this.state.data.state.nitta.process.steps}
          relations={this.state.data.state.nitta.process.relations}
        /> }
      </div>
    )
  }
}
