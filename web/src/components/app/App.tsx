import React from "react";
import { Route, Switch, Redirect } from "react-router-dom";

import { IAppContext, AppContextProvider, SID, sidSeparator } from "./AppContext";
import { AppNavbar } from "./AppNavbar";
import NotFoundErrorPage from "components/pages/errors/NotFoundErrorPage";

import { SynthesisGraphContainer } from "components/pages/synthesis/SynthesisGraphContainer";
import { NodeView } from "components/pages/node/NodeView";
import { SubforestView } from "components/pages/subforest/SubforestView";
import { TestBenchPage } from "components/pages/testBench/TestBenchPage";
import { ProcessView } from "components/pages/process/ProcessView";
import { DebugView } from "components/pages/debug/DebugView";

export interface IAppProps {}

// IMPORTANT: the value of AppContext.Provider MUST be {this.state} so React can handle re-rendering appropriately.
// It's sad, but it's the best option we have.
// And yes, you got it right, EVERYTHING you need in context (including functions) must be in this.state.
export type IAppState = IAppContext;

export default class App extends React.Component<IAppProps, IAppState> {
  constructor(props: IAppProps) {
    super(props);

    this.state = {
      selectedSID: sidSeparator,

      setSID: (sid: SID) => {
        this.setState({ selectedSID: sid });
      },

      resetSID: () => {
        this.setState({ selectedSID: sidSeparator });
      },
    };
  }

  public render() {
    return (
      <AppContextProvider value={this.state}>
        <AppNavbar />

        <div className="flex-grow-1">
          <SynthesisGraphContainer />

          <Switch>
            <Route exact path="/">
              <Redirect to="/node" />
            </Route>
            <Route exact path="/node">
              <NodeView />
            </Route>
            <Route exact path="/subforest">
              <SubforestView />
            </Route>
            <Route exact path="/process">
              <ProcessView />
            </Route>
            <Route exact path="/testbench">
              <TestBenchPage />
            </Route>
            <Route exact path="/debug">
              <DebugView />
            </Route>

            <Route component={NotFoundErrorPage} />
          </Switch>
        </div>
      </AppContextProvider>
    );
  }
}
