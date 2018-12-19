import React, { Component } from 'react'
import { hapi } from '../hapi'
import ReactTable from 'react-table'
import { LineChart } from 'react-easy-chart'

export class EdgesView extends Component {
  constructor (props) {
    super(props)
    this.onNIdChange = props.onNIdChange
    this.state = {
      selectedNId: props.selectedNId,
      options: null,
      edge: null
    }
    this.reloadEdges(props.selectedNId)
  }

  componentWillReceiveProps (props) {
    console.debug('EdgesView:componentWillReceiveProps(', props, ')')
    if (this.state.selectedNId !== props.selectedNId) this.reloadEdges(props.selectedNId)
    this.setState({selectedNId: props.selectedNId})
  }

  reloadEdges (nid) {
    if (nid === undefined || nid === null) return
    console.debug('EdgesView:reloadEdges(', nid, ')')
    hapi.getEdges(nid)
      .then(response => {
        this.setState({
          options: response.data.map(e => { return [e.eCharacteristic, e.eCharacteristics, e.eOption, e.eDecision] })
        })
      })
      .catch(err => console.log(err))
    hapi.getEdge(nid)
      .then(response => {
        this.setState({
          edge: response.data
        })
      })
      .catch(err => console.log(err))
  }

  render () {
    if (this.state.options === undefined || this.state.options === null) return <div />
    if (this.state.options.length === 0) return <pre> Process is over. Options not allow. </pre>

    return (
      <div>
        <pre>current edge:</pre>
        <small><pre>{ JSON.stringify(this.state.edge, null, 2) }</pre></small>
        <pre>sub edges:</pre>
        <div className='grid-x'>
          <div className='cell small-4'>
            <small><pre>{ JSON.stringify(this.state.options[0][1], null, 2) }</pre></small>
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
                    hapi.getNode(this.state.selectedNId === ':' ? ':' + row.index : this.state.selectedNId + ':' + row.index)
                      .then(response => {
                        this.onNIdChange(response.data.nId)
                      })
                      .catch(err => alert(err))
                  }}> { row.value }
                  </a>
              },
              {Header: 'Description', accessor: '2', Cell: row => <pre> {JSON.stringify(row.value)} </pre>},
              {Header: 'Metrics', accessor: '1', Cell: row => <pre> {JSON.stringify(row.value) } </pre>}
            ]
          }
          data={this.state.options}
        />
      </div>
    )
  }
}
