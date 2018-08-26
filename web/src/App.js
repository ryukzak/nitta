import React, { Component } from 'react'
import './App.css'
import 'react-table/react-table.css'
import { SynthesisGraph } from './components/synthesis-graph'
import { hapi } from './hapi'
import { LinkButton, showSRoot } from './utils'
import { StepView } from './components/step-view'

class App extends Component {
  constructor () {
    super()
    this.state = {
      currentSynthesis: null,
      refreshSynthesisGraph: true
    }
  }

  selectSynthesis (sRoot) {
    console.debug('App:selectSynthesis(', sRoot, ')')
    if (sRoot) {
      hapi.getSynthesis(sRoot)
        .then(response => {
          this.setState({
            currentSynthesis: sRoot,
            refreshSynthesisGraph: !this.state.refreshSynthesisGraph
          })
        })
        .catch(err => console.log(err))
    }
  }

  render () {
    return (
      <div>
        <nav aria-label='You are here:'>
          <ul className='breadcrumbs'>
            <li>Project</li>
            { this.state.currentSynthesis !== null && (<li> {this.state.currentSynthesis.sid}.{this.state.currentSynthesis.six} </li>) }
          </ul>
        </nav>
        <SynthesisGraph propagateSRoot={sRoot => this.selectSynthesis(sRoot)} refreshTrigger={this.state.refreshSynthesisGraph} />
        <SynthesisView sRoot={this.state.currentSynthesis} propagateSRoot={(sRoot) => this.selectSynthesis(sRoot)} />
      </div>
    )
  }
}

class SynthesisView extends Component {
  constructor (props) {
    super(props)
    this.state = {
      sRoot: props.sRoot,
      view: null,
      data: null
    }
    this.propagateSRoot = props.propagateSRoot
    if (props.sRoot !== undefined) this.handleSynthesisChange(props.sRoot)
  }

  handleSynthesisChange (sRoot, view) {
    console.debug('SynthesisView:handleSynthesisChange(', sRoot, view, ')')
    if (sRoot) {
      hapi.getSynthesis(sRoot)
        .then(response => {
          this.setState({
            sRoot: sRoot,
            data: response.data,
            view: view === undefined ? 'info' : view
          })
          this.propagateSRoot(sRoot)
        })
        .catch(err => console.log(err))
    }
  }

  componentWillReceiveProps (props) {
    if (this.state.sRoot !== props.sRoot) this.handleSynthesisChange(props.sRoot)
  }

  render (props) {
    return (
      <div>
        { this.state.sRoot === null && <pre> SYNTHESIS NOT SELECTED </pre> }
        { this.state.sRoot !== null &&
          <div className='tiny button-group'>
            <a className='button primary' onClick={() => this.setState({view: 'info'})}>info</a>
            { this.state.data.sSteps.map((step, i) =>
              <LinkButton key={i} sname={i}
                onClick={() => { this.setState({view: 'step', six: i}) }}
              />
            )}
            <a className='button primary' onClick={
              () => {
                hapi.compilerStep(this.state.sRoot, true)
                  .then(response => {
                    console.log(response.data)
                    this.handleSynthesisChange(response.data[0], response.data[1])
                  })
                  .catch(err => alert(err))
              }
            }>one step</a>
            <a className='button primary' onClick={
              () => {
                hapi.compilerStep(this.state.sRoot, false)
                  .then(response => {
                    console.log(response.data)
                    this.handleSynthesisChange(response.data[0], response.data[1])
                  })
                  .catch(err => alert(err))
              }
            }>all steps</a>
          </div>
        }
        { this.state.view === 'info' &&
          <SynthesisInfo
            data={this.state.data}
            propagateSRoot={sRoot => this.propagateSRoot(sRoot)}
          />
        }
        { this.state.view === 'step' &&
          <StepView sRoot={this.state.sRoot} six={this.state.six} propagateSRoot={sRoot => this.propagateSRoot(sRoot)} />
        }
      </div>
    )
  }
}

function SynthesisInfo (props) {
  return (
    <dl>
      <dt>Parent:</dt>
      <dd> { props.data.sParent
        ? (<LinkButton sname={showSRoot(props.data.sParent)} onClick={() => props.propagateSRoot(showSRoot(props.data.sParent))} />)
        : (<div> - </div>) }
      </dd>
      <dt>Childs:</dt>
      <dd>
        <div className='tiny button-group'>
          { props.data.sChilds.map((k, i) => <LinkButton key={i} sname={showSRoot(k)} onClick={() => props.propagateSRoot(k)} />) }
        </div>
      </dd>
      <hr />
      <dd>
        <pre>{ JSON.stringify(props.data, null, 2) }</pre>
      </dd>
    </dl>
  )
}

export default App
