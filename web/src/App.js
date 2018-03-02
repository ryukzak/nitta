import React, { Component } from 'react';
import './App.css';
import api from './gen/nitta-api.js';

class App extends Component {

  constructor () {
    super()
    this.state = { synthesis: [] }
    this.handleClick = this.handleClick.bind(this)
    this.handleClick()
  }


  handleClick () {
    api.getSynthesis()
    .then(response => this.setState({ synthesis: response.data }))
    .catch(err => console.log(err))
  }

  render() {
    return (
      <div className='button__container'>
        <button className='button' onClick={this.handleClick}>Click Me</button>
        Synthesis:
        <ul>
          { Object.keys(this.state.synthesis).map( k => <li key={k}> <Synthesis name={ k } parent={ this } /> </li> ) }
        </ul>
        <pre>
          { this.state.data }
        </pre>
      </div>
    );
  }
}



class Synthesis extends Component {
  constructor (props) {
    super()
    this.state = { name: props.name, parent: props.parent }
    this.handleClick = this.handleClick.bind(this);
  }

  handleClick () {
    api.getSynthesisBySid(this.state.name)
    .then(response => this.state.parent.setState({ data: JSON.stringify(response.data, null, 2) }))
    .catch(err => console.log(err))
  }
  
  render() {
    console.log(2, this.props.name)
    return ( <a onClick={this.handleClick}> { this.props.name } </a> )
  }
}

export default App;
