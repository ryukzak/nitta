import React from "react";
import { Route, Switch } from "react-router-dom";
import { Tabs, Tab } from "react-bootstrap"

import NotFoundErrorPage from "../pages/errors/NotFoundErrorPage";
import IndexPage from "../pages/main/MainPage";
import NodeView from "../pages/node/NodeView";
import { EdgesView } from "../pages/edges/EdgesView";
import ProcessPage from "../pages/process/ProcessPage";
import TestBenchPage from "../pages/testBench/TestBenchPage";

import AppNavbar from "./AppNavbar";
import AppFooter from "./AppFooter";

import { IAppContext, AppContextProvider, SelectedNodeId } from "./AppContext";

import "./AppTabs.scss";

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
        <div className="position-relative" style={{minHeight: "100vh"}}>
          <AppNavbar />

          <div style={{minHeight: "calc(100vh - 7.1em)", marginTop: "5.5em"}}>
            <Switch>
              <Route exact path="/"  >

                <IndexPage />
                <Tabs className="bg-dark fixed-top mt-5" defaultActiveKey="node" id="uncontrolled-tab-example" transition={false}>
                  <Tab tabClassName="tab" eventKey="node" title="NodeView" >
                    <NodeView selectedNId={this.state.selectedNodeId} />
                  </Tab>
                  <Tab tabClassName="tab" eventKey="edges" title="EdgesView">
                    <EdgesView nid={this.state.selectedNodeId} onNidChange={this.state.selectNode} />
                  </Tab>
                  <Tab tabClassName="tab" eventKey="process" title="ProcessView">
                    <ProcessPage nId={this.state.selectedNodeId} />
                  </Tab>
                  <Tab tabClassName="tab" eventKey="testBench" title="TestBench">
                    <TestBenchPage nId={this.state.selectedNodeId} />
                  </Tab>
                </Tabs>

              </Route>

              <Route component={NotFoundErrorPage} />
            </Switch>
          </div>

          <AppFooter />
        </div>
      </AppContextProvider>
    );
  }
}
