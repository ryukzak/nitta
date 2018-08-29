import React, { Component } from 'react'
import { hapi } from '../hapi'
import ReactTable from 'react-table'
import { LineChart } from 'react-easy-chart'

export class SimpleCompilerView extends Component {
  constructor (props) {
    super(props)
    this.onCurrentNidChange = props.onCurrentNidChange
    this.state = {
      currentNid: props.currentNid,
      options: null
    }
    this.updateSCOptions(props.currentNid)
  }

  componentWillReceiveProps (props) {
    console.debug('SimpleCompilerView:componentWillReceiveProps(', props, ')')
    if (this.state.currentNid !== props.currentNid) this.updateSCOptions(props.currentNid)
    this.setState({currentNid: props.currentNid})
  }

  updateSCOptions (nid) {
    if (nid === undefined || nid === null) return
    console.debug('SimpleCompilerView:updateSCOptions§(', nid, ')')
    hapi.simpleCompilerOptions(nid)
      .then(response => {
        this.setState({
          options: response.data
        })
      })
      .catch(err => console.log(err))
  }

  render () {
    if (this.state.options === undefined || this.state.options === null) return <div />
    if (this.state.options.length === 0) return <pre> Process is over. Options not allow. </pre>

    return (
      <div>
        <div className='grid-x'>
          <div className='cell small-4'>
            <pre>{ JSON.stringify(this.state.options[0][1], null, 2) }</pre>
          </div>
          <div className='cell small-8'>
            <LineChart data={[ this.state.options.map((e, index) => { return { x: index, y: e[0] } }) ]}
              width={750} height={250}
              axes />
          </div>
        </div>
        <ReactTable
          columns={
            [
              {
                Header: 'Integral',
                accessor: '0',
                maxWidth: 70,
                Cell: row =>
                  <a onClick={() => {
                    hapi.manualDecision(this.state.currentNid, row.index)
                      .then(response => {
                        this.onCurrentNidChange(response.data)
                      })
                      .catch(err => alert(err))
                  }}> { row.value }
                  </a>
              },
              {Header: 'Description', accessor: '3', Cell: row => <pre> {JSON.stringify(row.value)} </pre>},
              {Header: 'Metrics', accessor: '2', Cell: row => <pre> {JSON.stringify(row.value) } </pre>}
            ]
          }
          data={this.state.options}
        />
      </div>
    )
  }
}
