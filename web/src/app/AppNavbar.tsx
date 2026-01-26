import type { FC } from "react";
import { Nav, Navbar } from "react-bootstrap";
import { Link, NavLink as RouterNavLink } from "react-router-dom";

import "./AppNavbar.scss";

import { AppNavbarButtons } from "./AppNavbarButtons";

export type IAppNavbarProps = {};

export const AppNavbar: FC<IAppNavbarProps> = (_props) => {
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
          <Nav.Link as={RouterNavLink} to="/node">
            Node
          </Nav.Link>
          <Nav.Link as={RouterNavLink} to="/subforest">
            Subforest
          </Nav.Link>
          <Nav.Link as={RouterNavLink} to="/process">
            Process
          </Nav.Link>
          <Nav.Link as={RouterNavLink} to="/process2">
            Process#2
          </Nav.Link>
          <Nav.Link as={RouterNavLink} to="/testbench">
            Testbench
          </Nav.Link>
          <Nav.Link as={RouterNavLink} to="/debug">
            Debug
          </Nav.Link>
        </Nav>
      </Navbar.Collapse>

      <AppNavbarButtons />

      <Navbar.Toggle aria-controls="navbar-nav" />
    </Navbar>
  );
};
