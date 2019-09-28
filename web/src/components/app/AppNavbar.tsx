import * as React from "react";
import { Navbar, Nav, NavDropdown } from "react-bootstrap";
import { NavLink, Link } from "react-router-dom";

import "./AppNavbar.scss";

export interface IAppNavbarProps {}

export interface IAppNavbarState {}

export default class AppNavbar extends React.Component<IAppNavbarProps, IAppNavbarState> {
  constructor(props: IAppNavbarProps) {
    super(props);

    this.state = {};
  }

  public render() {
    return (
      <Navbar bg="dark" variant="dark" expand="md" sticky="top" className="appNavbar">
        <Navbar.Brand className="mr-md-5 ml-4" as={Link} to="/">
          <img alt="Prsoject Logo" src="/itmo_logo.png" height="30" className="mr-2 d-inline-block align-center"></img>
          CPS Lab Frontend
        </Navbar.Brand>
        <Navbar.Toggle aria-controls="navbar-nav" />
        <Navbar.Collapse id="navbar-nav">
          <Nav className="mr-auto">
            {/* "as" property can help us to integrate React Router's NavLink */}
            <Nav.Link as={NavLink} exact to="/">
              Home
            </Nav.Link>
            <Nav.Link as={NavLink} to="/todos/">
              Todos
            </Nav.Link>
            <Nav.Link as={NavLink} to="/graph/">
              Graph
            </Nav.Link>
            <NavDropdown title="About Us" id="nav-dropdown">
              <NavDropdown.Item as={NavLink} to="/about/tos">
                Terms and Conditions
              </NavDropdown.Item>
              <NavDropdown.Item as={NavLink} to="/about/privacy">
                Privacy Policy
              </NavDropdown.Item>
              <NavDropdown.Divider />
              <NavDropdown.Item as={NavLink} to="/about/jobs">
                Jobs
              </NavDropdown.Item>
              <NavDropdown.Item as={NavLink} to="/about/contacts">
                Contacts
              </NavDropdown.Item>
            </NavDropdown>
          </Nav>
        </Navbar.Collapse>
      </Navbar>
    );
  }
}
