import React, { Component } from 'react'
import './App.css'
import 'react-table/react-table.css'

export function LinkButton (props) {
  return (
    <a className='button tiny secondary' onClick={props.onClick}> {props.sname} </a>
  )
}
