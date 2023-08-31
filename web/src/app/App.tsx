import React, { Component } from "react";
import { Redirect, Route, Switch } from "react-router-dom";

import { Sid, sidSeparator } from "services/HaskellApiService";
import { AppContextProvider, IAppContext } from "./AppContext";
import { AppNavbar } from "./AppNavbar";

import { SynthesisGraph } from "components/SynthesisGraph";
import { DebugScreen } from "screens/DebugScreen";
import { NodeScreen } from "screens/NodeScreen";
import { ProcessScreen } from "screens/ProcessScreen";
import { SubforestScreen } from "screens/SubforestScreen";
import { TestBenchScreen } from "screens/TestBenchScreen";

export interface IAppProps {}

// IMPORTANT: the value of AppContext.Provider MUST be {this.state} so React can handle re-rendering appropriately.
// It's sad, but it's the best option we have.
// And yes, you got it right, EVERYTHING you need in context (including functions) must be in this.state.
export type IAppState = IAppContext;

export default class App extends Component<IAppProps, IAppState> {
  constructor(props: IAppProps) {
    super(props);

    this.state = {
      selectedSid: sidSeparator,

      setSid: (sid: Sid) => {
        this.setState({ selectedSid: sid });
      },

      resetSid: () => {
        this.setState({ selectedSid: sidSeparator });
      },
    };
  }

  public render() {
    return (
      <AppContextProvider value={this.state}>
        <AppNavbar />

        <div className="flex-grow-1">
          <SynthesisGraph />
          <Switch>
            <Route exact path="/">
              <Redirect to="/node" />
            </Route>
            <Route exact path="/node">
              <NodeScreen />
            </Route>
            <Route exact path="/subforest">
              <SubforestScreen />
            </Route>
            <Route exact path="/process">
              <ProcessScreen />
            </Route>
            <Route exact path="/testbench">
              <TestBenchScreen />
            </Route>
            <Route exact path="/debug">
              <DebugScreen />
            </Route>

            <Route>404 NOT FOUND</Route>
          </Switch>
        </div>
      </AppContextProvider>
    );
  }
}
