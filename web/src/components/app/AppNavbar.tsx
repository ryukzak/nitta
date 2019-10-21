import * as React from "react";
import { Navbar, Nav} from "react-bootstrap";
import { Link } from "react-router-dom";

import "./AppNavbar.scss";

import {AppContextConsumer} from "./AppContext";

export interface IAppNavbarProps { }

export interface IAppNavbarState { }

export default class AppNavbar extends React.Component<IAppNavbarProps, IAppNavbarState> {

  constructor(props: IAppNavbarProps) {
    super(props);

    this.state = {};
  }

  public render() {
    return (
      <AppContextConsumer>
        {appContext => appContext && (
          <Navbar bg="dark" variant="dark" expand="md" fixed="top" className="appNavbar">
          <Navbar.Brand className="mr-md-5 " as={Link} to="/">
            <img alt="Project Logo" src="/logo_white.png" height="35" className="mr-2 d-inline-block align-center mb-1"></img>
            NITTA Web UI
          </Navbar.Brand>
          <Navbar.Collapse id="navbar-nav">
            <Nav className="mr-auto">
              {/* "as" property can help us to integrate React Router's NavLink */}
              {/* <Nav.Link as={NavLink} exact to="/">
                Home
              </Nav.Link> */}
            </Nav>
          </Navbar.Collapse>
          <Navbar.Text className="mr-5 text-white">Selected Node ID: {appContext.selectedNodeId || "None"}</Navbar.Text>
          <Navbar.Toggle aria-controls="navbar-nav" />
        </Navbar>
        )}
      </AppContextConsumer>
    );
  }
}
