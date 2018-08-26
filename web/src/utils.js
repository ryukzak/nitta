import React, { Component } from 'react'
import './App.css'
import 'react-table/react-table.css'

export function showSRoot (sRoot, six) {
  console.log(sRoot)
  if (six === undefined) return sRoot.sid + '[' + sRoot.six + ']'
  return sRoot.sid + '[' + sRoot.six + ':' + six + ']'
}

export function LinkButton (props) {
  return (
    <a className='button tiny secondary' onClick={props.onClick}> {props.sname} </a>
  )
}
