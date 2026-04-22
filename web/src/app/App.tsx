import { SynthesisGraph } from "components/SynthesisGraph";
import { SplitPane } from "components/utils/SplitPane";
import { Component } from "react";
import { Navigate, Route, Routes } from "react-router-dom";
import { DebugScreen } from "screens/DebugScreen";
import { NodeScreen } from "screens/NodeScreen";
import { Process2Screen } from "screens/Process2Screen";
import { ProcessScreen } from "screens/ProcessScreen";
import { SubforestScreen } from "screens/SubforestScreen";
import { TestBenchScreen } from "screens/TestBenchScreen";
import { type Sid, sidSeparator } from "services/HaskellApiService";
import { AppContextProvider, type IAppContext } from "./AppContext";
import { AppNavbar } from "./AppNavbar";

export type IAppProps = {};

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
        <div
          className="app-content"
          style={{ flex: 1, minHeight: 0, display: "flex", overflow: "hidden" }}
        >
          {/* style to apply certain overflow behaviour only to this certain child */}
          <style>
            {`
              .app-content {
                > .split-pane-container {
                  > .split-pane-right {
                    overflow-y: auto;
                    overflow-x: hidden;
                  }
                }
              }
            `}
          </style>
          <SplitPane orientation="horizontal" initialSplitPercentage={35}>
            <SynthesisGraph />
            <div
              className="h-full overflow-auto"
              style={{ backgroundColor: "white" }}
            >
              <Routes>
                <Route
                  path="/"
                  element={<Navigate to="/node"></Navigate>}
                ></Route>
                <Route path="/node" element={<NodeScreen />}></Route>
                <Route path="/subforest" element={<SubforestScreen />}></Route>
                <Route path="/process" element={<ProcessScreen />}></Route>
                <Route path="/process2" element={<Process2Screen />}></Route>
                <Route path="/testbench" element={<TestBenchScreen />}></Route>
                <Route path="/debug" element={<DebugScreen />}></Route>
                <Route>404 NOT FOUND</Route>
              </Routes>
            </div>
          </SplitPane>
        </div>
      </AppContextProvider>
    );
  }
}
