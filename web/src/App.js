import React, { Component } from 'react';
import './App.css';
import api from './gen/nitta-api.js';

class App extends Component {

  constructor() {
    super()
    this.state = { synthesisList: [] 
                 , path: []
                 , synthesis: null 
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
    api.getSynthesisBySid(sname)
    .then( response => {
      this.setState({ synthesis: response.data
                    , path: [ sname ]
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

        <Synthesis data={ this.state.synthesis } app={ this } />
      </div>
    )
  }
}

function SynthesisLink(props) {
  return (
    <a className="button tiny secondary" href="#" onClick={ props.onClick }> { props.sname } </a>
  )
}

function Synthesis(props) {
  var data = props.data
  if (props.data)
    return (
      <div>
        <dl>
          <dt>Parent:</dt>
          <dd> { data.parent ? ( <SynthesisLink sname={ data.parent[0] } onClick={ () => props.app.getSynthesis( data.parent[0] ) } />  )
                             : ( <div> - </div> ) } </dd>
          <dt>Childs:</dt>
          <dd> 
            <div className="tiny button-group">
              { data.childs.map( (k, i) => <SynthesisLink key={ i } sname={ k } onClick={ () => props.app.getSynthesis(k) }/> ) }
            </div>
          </dd>
          <dt>Config (may vari from step to step):</dt>
          <dd>
            <pre>{ JSON.stringify(data.config, null, 2) }</pre>
          </dd>
          <dt>States:</dt>
          <dd>
            <div className="tiny button-group">
              { data.states.map( (st, i) => <SynthesisLink key={ i } sname={ i } /> ) }
            </div>
          </dd>
        </dl>

        <pre> { JSON.stringify(data, null, 2) } </pre>
      </div>
    )
  else return <pre> SYNTHESIS NOT SELECTED </pre>
}

export default App;
