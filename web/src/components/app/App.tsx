import React from "react";
import { Route, Switch } from "react-router-dom";
import NotFoundErrorPage from "../pages/errors/NotFoundErrorPage";
import IndexPage from "../pages/main/MainPage";
import AppNavbar from "./AppNavbar";
import AppFooter from "./AppFooter";
import AppContext, { SelectedNodeId, IAppContext } from "./AppContext";

export interface IAppProps {}

// IMPORTANT: the value of AppContext.Provider MUST be {this.state} so React can handle re-rendering appropriately.
// It's sad, but it's the best option we have.
// And yes, you got it right, EVERYTHING you need in context (including functions) must be in this.state.
export type IAppState = IAppContext;

export default class App extends React.Component<IAppProps, IAppState> {
  constructor(props: IAppProps) {
    super(props);

    this.state = {
      synthesisGraphHeight: 0,
      minSynthesisGraphHeight: 0,
      selectedNodeId: null,

      selectNode: (id: SelectedNodeId) => {
        this.setState({ selectedNodeId: id });
      },

      changedSynthesisGraphHeight: (newSynthesisGraphHeight: number) => {
        this.setState({ synthesisGraphHeight: newSynthesisGraphHeight })
      },

      reloadSelectedNode: () => {
        // TODO: implement any actions like this if needed, giving this function for use in AppContext.
        console.log("Reloading selected node.");
        this.setState({ selectedNodeId: null });
      },
    };
  }

  public render() {
    return (
      <AppContext.Provider value={this.state}>
        <AppNavbar />

        <div className="flex-grow-1">
          <Switch>
            <Route exact path="/" component={IndexPage} />
            <Route component={NotFoundErrorPage} />
          </Switch>
        </div>

        <AppFooter />
      </AppContext.Provider>
    );
  }
}
