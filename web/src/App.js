import React, { Component } from 'react';
import './App.css';
import api from './gen/nitta-api.js';
import ReactTable from 'react-table'
import 'react-table/react-table.css'

class App extends Component {

  constructor() {
    super()
    this.state = { synthesisList: [] 
                 , path: []
                 , synthesis: null
                 , step: null
                 }
    this.getAllSynthesis = this.getAllSynthesis.bind(this)
    this.getAllSynthesis()
  }

  getAllSynthesis() {
    api.getSynthesis()
    .then(response => this.setState({ synthesisList: Object.keys(response.data)
                                    , path: []
                                    , synthesis: null 
                                    }))
    .catch(err => console.log(err))
  }

  getSynthesis(sname) {
    api.getSynthesisBySId(sname)
    .then( response => {
      this.setState({ synthesis: response.data
                    , path: [ sname ]
                    , step: null
                    })
    } )
    .catch( err => console.log(err) )
  }

  getStep(sname, did) {
    api.getSynthesisBySIdStepsByStepId(sname, did)
    .then( response => {
      this.setState({ step: response.data
                    , path: [ sname, did ]
                    })
    } )
    .catch( err => console.log(err) )
  }


  render() {
    return (
      <div>
        <nav aria-label="You are here:" role="navigation">
          <ul className="breadcrumbs">
            <li>Project</li>
            { this.state.path.map( (v, i) => <li key={ i }> { v } </li> ) }
          </ul>
        </nav>        

        <div className="tiny button-group">
          <a className="button primary" onClick={ this.getAllSynthesis }>Refresh</a>
          { this.state.synthesisList.map( (sname, i) => 
            <SynthesisLink key={ i } sname={ sname } onClick={ () => this.getSynthesis(sname) } /> ) }
        </div>

        <View synthesisData={ this.state.synthesis } stepData={ this.state.step } app={ this } />
      </div>
    )
  }
}

function SynthesisLink(props) {
  return (
    <a className="button tiny secondary" href="#" onClick={ props.onClick }> { props.sname } </a>
  )
}

function View(props) {
  var sdata = props.synthesisData
  return (
    <div>
      { (props.synthesisData)
        ? <div>
            <dl>
              <dt>Parent:</dt>
              <dd> { sdata.parent ? ( <SynthesisLink sname={ sdata.parent[0] } onClick={ () => props.app.getSynthesis( sdata.parent[0] ) } />  )
                                 : ( <div> - </div> ) } </dd>
              <dt>Childs:</dt>
              <dd> 
                <div className="tiny button-group">
                  { sdata.childs.map( (k, i) => <SynthesisLink key={ i } sname={ k } onClick={ () => props.app.getSynthesis(k) }/> ) }
                </div>
              </dd>
              <dt>Config (may vary from step to step):</dt>
              <dd>
                <pre>{ JSON.stringify(sdata.config, null, 2) }</pre>
              </dd>
              <dt>States:</dt>
              <dd>
                <div className="tiny button-group">
                  { sdata.steps.map( (st, i) => <SynthesisLink key={ i } sname={ i } onClick={ () => props.app.getStep(props.app.state.path[0], i) } /> ) }
                </div>
              </dd>
            </dl>
          </div>
        : <pre> SYNTHESIS NOT SELECTED </pre>
      }
      { ( props.stepData )
        ? <div>
            <div className="tiny primary button-group">
              {/* <SynthesisLink sname="fork" onClick={ () => props.app.getStep(props.app.state.path[0]) } /> */}
              {/* <SynthesisLink sname="root" onClick={ () => props.app.getStep(props.app.state.path[0]) } /> */}
              {/* <SynthesisLink sname="decisions" onClick={ () => props.app.getStep(props.app.state.path[0]) } /> */}
              <SynthesisLink sname="options" onClick={ 
                () => { 
                  api.getSynthesisBySIdStepsByStepIdOptions( props.app.state.path[0], props.app.state.path[1] )
                  .then( response => props.app.setState( { step: response.data
                                                         , path: [ props.app.state.path[0]
                                                                 , props.app.state.path[1]
                                                                 , "options"
                                                                 ] 
                                                         } ) )
                } } />
              <SynthesisLink sname="mk_decision" onClick={ 
                () => {
                  api.postSynthesisBySIdStepsByStepIdAuto(props.app.state.path[0], props.app.state.path[1] + 1)
                  .then( response => { 
                    props.app.getSynthesis(props.app.state.path[0]) 
                    props.app.getStep(props.app.state.path[0], props.app.state.path[1] + 1) 
                  } )
                  .catch( err => alert(err))
                } } />
            </div>
            { ( props.app.state.path[2] == 'options' )
              ? <ReactTable columns={ [ { Header: 'Type', accessor: '3.tag' }
                                      , { Header: 'Description', accessor: '3.contents' }
                                      , { Header: 'Integral', accessor: "0" }
                                      // TODO: generators for other metrics.
                                      ]
                                    }
                            data={props.stepData} />
              : <pre>
                  { JSON.stringify(props.stepData, null, 2) }
                </pre>

            }
          </div>
        : <pre> STEP NOT SELECTED </pre>
      }
    </div>
  )
}

export default App;
