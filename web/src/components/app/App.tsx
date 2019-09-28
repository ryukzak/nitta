import React from "react";
import { Route, Switch } from "react-router-dom";
import NotFoundErrorPage from "../pages/errors/NotFoundErrorPage";
import IndexPage from "../pages/index/IndexPage";
import AppNavbar from "./AppNavbar";
import AppFooter from "./AppFooter";

// TODO: AppContext with selectedNId

export default class App extends React.Component {
  public render() {
    return (
      <>
        <AppNavbar />

        <Switch>
          <Route exact path="/" component={IndexPage} />
          <Route component={NotFoundErrorPage} />
        </Switch>

        <AppFooter />
      </>
    );
  }
}
