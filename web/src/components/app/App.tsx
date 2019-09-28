import React from "react";
import { Route, Switch } from "react-router-dom";
import NotFoundErrorPage from "../pages/errors/NotFoundErrorPage";
import GraphPage from "../pages/graph/GraphPage";
import IndexPage from "../pages/index/IndexPage";
import TodosPage from "../pages/todos/TodosPage";
import AppNavbar from "./AppNavbar";
import AppFooter from "./AppFooter";

export interface IAppProps {}

export default class App extends React.Component<IAppProps> {
  public render() {
    return (
      <>
        <AppNavbar />

        <Switch>
          <Route exact path="/" component={IndexPage} />
          <Route exact path="/todos/" component={TodosPage} />
          <Route exact path="/graph/" component={GraphPage} />
          <Route component={NotFoundErrorPage} />
        </Switch>

        <AppFooter />
      </>
    );
  }
}
