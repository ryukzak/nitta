import React, { Component } from 'react'
import './App.css'
import ReactTable from 'react-table'
import 'react-table/react-table.css'
import { LineChart } from 'react-easy-chart'
import { SynthesisGraph } from './components/synthesis-graph'
import { ProcessView } from './components/process-view'
import { hapi } from './hapi'
import { LinkButton } from './utils'

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
          // <pre>{JSON.stringify(this.state.step, null, 2) }</pre>
        }
      </div>
    )
  }
}

class StepView extends Component {
  constructor (props) {
    super(props)
    this.state = {
      sRoot: props.sRoot,
      six: props.six,
      data: null
    }
    this.propagateSRoot = props.propagateSRoot
    this.handleSixChange(props.sRoot, props.six)
  }

  componentWillReceiveProps (props) {
    if (this.state.sRoot !== props.sRoot ||
      this.state.six !== props.six) this.handleSixChange(props.sRoot, props.six)
  }

  handleSixChange (sRoot, six) {
    console.debug('StepView:handleSixChange(', sRoot, six, ')')
    if (sRoot !== null && six !== null && sRoot !== undefined && six !== undefined) {
      hapi.getStep2(sRoot, six)
        .then(response => {
          this.setState({
            sRoot: sRoot,
            six: six,
            data: response.data
          })
        })
        .catch(err => console.log(err))
    }
  }

  forkSynthesis (sRoot, six) {
    if (sRoot.six > six) {
      alert('if six is from previous synthesis, fork must be start early.')
      return
    }
    // FIXME: if six is from previous synthesis, fork must be start early.
    hapi.forkSynthesis(sRoot, six)
      .then(response => {
        this.propagateSRoot(response.data)
      })
      .catch(err => console.log(err))
  }

  render () {
    return (
      <div>
        <div className='tiny primary button-group'>
          {/* <LinkButton sname='options' onClick={
            () => app.getStepOption(path.sId, path.stepId)} />
          <LinkButton sname='info' onClick={
            () => app.setPath(path.sId, 'step', path.stepId, 'info')} /> */}
          {/* <LinkButton sname='process' onClick={
            () => app.setPath(path.sId, 'step', path.stepId, 'process')} /> */}
          <LinkButton sname='fork' onClick={() => { this.forkSynthesis(this.state.sRoot, this.state.six) }} />
        </div>
        <pre>{ JSON.stringify(this.state.data, null, 2) }</pre>
        <hr />
        {/* { (path.stepView === 'options')
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
        } */}
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

function showSRoot (sRoot, six) {
  console.log(sRoot)
  if (six === undefined) return sRoot.sid + '[' + sRoot.six + ']'
  return sRoot.sid + '[' + sRoot.six + ':' + six + ']'
}

// function StepOptionView (props) {
//   var path = props.path
//   var opts = props.stepDataOptions
//   if (opts.length === 0) return <pre> Process is over. Options not allow. </pre>
//   return (
//     <div>
//       <div className='grid-x'>
//         <div className='cell small-4'>
//           <pre>{ JSON.stringify(opts[0][1], null, 2) }</pre>
//         </div>
//         <div className='cell small-8'>
//           <LineChart data={[ opts.map((e, index) => { return { x: index, y: e[0] } }) ]}
//             width={750} height={250}
//             axes />
//         </div>
//       </div>
//       <ReactTable
//         columns={
//           [
//             {
//               Header: 'Integral',
//               accessor: '0',
//               maxWidth: 70,
//               Cell: row =>
//                 <a onClick={
//                   () => {
//                     hapi.manualDecision(path.sId, path.stepId + 1, row.index)
//                       .then(response => {
//                         // FIXME: Не обновляется список синтезов.
//                         props.app.getSynthesis(path.sId)
//                         props.app.getStep(path.sId, path.stepId + 1)
//                       })
//                       .catch(err => alert(err))
//                   }}> { row.value }
//                 </a>
//             },
//             {Header: 'Description', accessor: '3', Cell: row => <pre> {JSON.stringify(row.value)} </pre>},
//             {Header: 'Metrics', accessor: '2', Cell: row => <pre> {JSON.stringify(row.value) } </pre>}
//           ]
//         }
//         data={opts}
//       />
//     </div>
//   )
// }

export default App
