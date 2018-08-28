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
      currentSynthesis: null
    }
  }

  selectSynthesis (sNode) {
    console.debug('App:selectSynthesis(', sNode, ')')
    if (sNode && sNode !== this.state.currentSynthesis) {
      this.setState({
        currentSynthesis: sNode
      })
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
        <SynthesisGraph currentNid={this.state.currentSynthesis} propagateSRoot={sRoot => this.selectSynthesis(sRoot)} />
        {/* <SynthesisView sRoot={this.state.currentSynthesis} propagateSRoot={sRoot => this.selectSynthesis(sRoot)} /> */}
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

  handleSynthesisChange (sRoot, view, six) {
    console.debug('SynthesisView:handleSynthesisChange(', sRoot, view, ')')
    if (sRoot) {
      hapi.getSynthesis(sRoot)
        .then(response => {
          this.setState({
            sRoot: sRoot,
            data: response.data,
            view: view === undefined ? 'info' : view,
            six: view === 'step' ? six : this.state.six
          })
          this.propagateSRoot(sRoot)
        })
        .catch(err => console.log(err))
    }
  }

  componentWillReceiveProps (props) {
    if (this.state.sRoot !== props.sRoot) this.handleSynthesisChange(props.sRoot)
  }

  compilerStep (oneStep) {
    hapi.compilerStep(this.state.sRoot, oneStep)
      .then(response => {
        this.handleSynthesisChange(response.data[0], 'step', response.data[1])
      })
      .catch(err => alert(err))
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
            <a className='button primary' onClick={() => { this.compilerStep(true) }}>one step</a>
            <a className='button primary' onClick={() => { this.compilerStep(false) }}>all steps</a>
          </div>
        }
        { this.state.view === 'info' && <SynthesisInfo
          data={this.state.data}
          propagateSRoot={sRoot => this.propagateSRoot(sRoot)}
        /> }
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
