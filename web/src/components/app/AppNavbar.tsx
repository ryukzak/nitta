import * as React from "react";
import { Navbar, Nav } from "react-bootstrap";
import { Link, NavLink as RouterNavLink } from "react-router-dom";

import "./AppNavbar.scss";

import { AppNavbarButtons } from "./AppNavbarButtons";

export interface IAppNavbarProps {}

export const AppNavbar: React.FC<IAppNavbarProps> = (props) => {
  return (
    <Navbar bg="dark" variant="dark" expand="md" className="appNavbar">
      <Navbar.Brand className="mr-md-5 " as={Link} to="/">
        <img
          alt="Project Logo"
          src="/logo_white.png"
          height="25"
          className="mr-2 d-inline-block align-center mb-1"
        ></img>
        NITTA
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
          <Nav.Link as={RouterNavLink} exact to="/debug">
            Debug
          </Nav.Link>
        </Nav>
        <AppNavbarButtons />
      </Navbar.Collapse>
      {/* <Navbar.Text className="mr-5 text-white">
              Selected Node ID: {appContext.selectedNodeId || "None"}
              </Navbar.Text> */}
      <Navbar.Toggle aria-controls="navbar-nav" />
    </Navbar>
  );
};