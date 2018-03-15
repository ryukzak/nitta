import React, { Component } from 'react';
import './App.css';
import api from './gen/nitta-api.js';
import ReactTable from 'react-table'
import 'react-table/react-table.css'
import {LineChart} from 'react-easy-chart';

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
                  { sdata.childs.map( (k, i) => <SynthesisLink key={ i } sname={ k[0] } onClick={ () => props.app.getSynthesis(k[0]) }/> ) }
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
              <SynthesisLink sname="fork" onClick={ 
                () => {
                  var sId = props.app.state.path[0] + "." + props.app.state.path[1]
                  while ( props.app.state.synthesisList.indexOf( sId ) >= 0 ) {
                    sId += "'"
                  }
                  api.postSynthesisBySId( sId
                                        , props.app.state.path[0]
                                        , props.app.state.path[1] 
                                        )
                  props.app.getAllSynthesis()
                  props.app.getSynthesis( sId )
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
              ? <div>
                  <div class="grid-x">
                    <div class="cell small-4">
                      <pre>{ JSON.stringify(props.stepData[0][1], null, 2) }</pre>
                    </div>
                    <div class="cell small-8">
                      <LineChart data={ [ props.stepData.map( (e, index) => { return { x: index, y: e[0] } }) ] }
                                 width={750} height={250} 
                                 axes />
                    </div>
                  </div>

                  <ReactTable columns={ [ { Header: 'Integral'
                                          , accessor: "0"
                                          , maxWidth: 70 
                                          , Cell: row => 
                                            <a onClick={ 
                                              () => {
                                                api.postSynthesisBySIdStepsByStepId( props.app.state.path[0], props.app.state.path[1] + 1, row.index )
                                                .then( response => { 
                                                  props.app.getSynthesis(props.app.state.path[0]) 
                                                  props.app.getStep(props.app.state.path[0], props.app.state.path[1] + 1) 
                                                } )
                                                .catch( err => alert(err))
                                              } }> { row.value } </a>
                                          }
                                        , { Header: 'Description', accessor: '3', Cell: row => <pre> { JSON.stringify(row.value) } </pre>  }
                                        , { Header: 'Metrics'
                                          , accessor: "2"
                                          , Cell: row => <pre> { JSON.stringify(row.value) } </pre> 
                                          }
                                        ]      
                                      }
                              data={props.stepData} />
                </div>
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
