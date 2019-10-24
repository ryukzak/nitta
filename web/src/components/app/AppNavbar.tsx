import * as React from "react";
import { Navbar, Nav } from "react-bootstrap";
import { Link, NavLink as RouterNavLink } from "react-router-dom";

import "./AppNavbar.scss";

import { AppContextConsumer } from "./AppContext";
import SynthesisButtonView from "../pages/main/SynthesisButtonView";

export interface IAppNavbarProps {}

export interface IAppNavbarState {}

export default class AppNavbar extends React.Component<IAppNavbarProps, IAppNavbarState> {
  constructor(props: IAppNavbarProps) {
    super(props);

    this.state = {};
  }

  public render() {
    return (
      <AppContextConsumer>
        {appContext =>
          appContext && (
            <Navbar bg="dark" variant="dark" expand="md" className="appNavbar">
              <Navbar.Brand className="mr-md-5 " as={Link} to="/">
                <img
                  alt="Project Logo"
                  src="/logo_white.png"
                  height="35"
                  className="mr-2 d-inline-block align-center mb-1"
                ></img>
                NITTA Web UI
              </Navbar.Brand>
              <Navbar.Collapse id="navbar-nav">
                <Nav className="mr-auto">
                  {/* "as" property can help us to integrate React Router's NavLink */}
                  <Nav.Link as={RouterNavLink} exact to="/node">
                    Node
                  </Nav.Link>
                  <Nav.Link as={RouterNavLink} exact to="/edges">
                    Edges
                  </Nav.Link>
                  <Nav.Link as={RouterNavLink} exact to="/process">
                    Process
                  </Nav.Link>
                  <Nav.Link as={RouterNavLink} exact to="/testbench">
                    Testbench
                  </Nav.Link>
                </Nav>
              </Navbar.Collapse>
              {/* <Navbar.Text className="mr-5 text-white">
                Selected Node ID: {appContext.selectedNodeId || "None"}
              </Navbar.Text> */}
              <SynthesisButtonView selectedNodeId={appContext.selectedNodeId} selectNode={appContext.selectNode} />
              <Navbar.Toggle aria-controls="navbar-nav" />
            </Navbar>
          )
        }
      </AppContextConsumer>
    );
  }
}
