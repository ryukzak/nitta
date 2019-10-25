import React from "react";
import { Route, Switch, Redirect } from "react-router-dom";

import NotFoundErrorPage from "../pages/errors/NotFoundErrorPage";
import NodeView from "../pages/node/NodeView";
import { EdgesView } from "../pages/edges/EdgesView";
import ProcessPage from "../pages/process/ProcessPage";
import { TestBenchPage } from "../pages/testBench/TestBenchPage";

import { IAppContext, AppContextProvider, SelectedNodeId } from "./AppContext";
import { AppNavbar } from "./AppNavbar";
import { AppFooter } from "./AppFooter";
import { MainPage } from "../pages/main/MainPage";

export interface IAppProps {}

// IMPORTANT: the value of AppContext.Provider MUST be {this.state} so React can handle re-rendering appropriately.
// It's sad, but it's the best option we have.
// And yes, you got it right, EVERYTHING you need in context (including functions) must be in this.state.
export type IAppState = IAppContext;

export default class App extends React.Component<IAppProps, IAppState> {
  constructor(props: IAppProps) {
    super(props);

    this.state = {
      selectedNodeId: "-",

      selectNode: (id: SelectedNodeId) => {
        this.setState({ selectedNodeId: id });
      },

      reloadSelectedNode: () => {
        this.setState({ selectedNodeId: null });
      },
    };
  }

  public render() {
    return (
      <AppContextProvider value={this.state}>
        <AppNavbar />

        <div className="flex-grow-1">
          <MainPage />

          <Switch>
            <Route exact path="/">
              <Redirect to="/node" />
            </Route>
            <Route exact path="/node">
              <NodeView selectedNId={this.state.selectedNodeId} />
            </Route>
            <Route exact path="/edges">
              <EdgesView nid={this.state.selectedNodeId} onNidChange={this.state.selectNode} />
            </Route>
            <Route exact path="/process">
              <ProcessPage nId={this.state.selectedNodeId} />
            </Route>
            <Route exact path="/testbench">
              <TestBenchPage />
            </Route>

            <Route component={NotFoundErrorPage} />
          </Switch>
        </div>

        <AppFooter />
      </AppContextProvider>
    );
  }
}
