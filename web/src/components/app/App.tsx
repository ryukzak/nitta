import React from "react";
import { Route, Switch } from "react-router-dom";
import { Tabs, Tab } from "react-bootstrap"
import NotFoundErrorPage from "../pages/errors/NotFoundErrorPage";
import IndexPage from "../pages/main/MainPage";
import NodePage from "../pages/node/NodePage";
import ProcessPage from "../pages/process/ProcessPage";
import TestBenchPage from "../pages/testBench/TestBenchPage";
import EdgesViewPage from "../pages/edges/EdgesViewPage"
import AppNavbar from "./AppNavbar";
import AppFooter from "./AppFooter";
import { IAppContext, AppContextProvider, SelectedNodeId } from "./AppContext";

import "./AppNavbar.scss";

export interface IAppProps { }

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
        // TODO: implement any actions like this if needed, giving this function for use in AppContext.
        this.setState({ selectedNodeId: null });
      },
    };
    
  }

  public render() {
    return (
      <AppContextProvider value={this.state}>
        <AppNavbar />

        <div className="flex-grow-1" >
          <Switch>
            <Route exact path="/" >

              <Tabs defaultActiveKey="home" id="uncontrolled-tab-example" transition={false}> 
                <Tab tabClassName="h-100" eventKey="home" title="SynthesisGraph" >
                  <IndexPage />
                </Tab>
                <Tab eventKey="node" title="NodeView">
                  <NodePage />
                </Tab>
                <Tab eventKey="edges" title="EdgesView">
                  <EdgesViewPage />
                </Tab>
                <Tab eventKey="process" title="ProcessView">
                  <ProcessPage nId={this.state.selectedNodeId} />
                </Tab>
                <Tab eventKey="testBench" title="TestBench">
                  <TestBenchPage nId={this.state.selectedNodeId} />
                </Tab>
              </Tabs>

            </Route>

            <Route component={NotFoundErrorPage} />
          </Switch>
        </div>

        <AppFooter />
      </AppContextProvider>
    );
  }
}
