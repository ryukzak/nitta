import * as React from "react";
import { haskellApiService } from "../../../services/HaskellApiService";
import { TestbenchReport } from "../../../gen/types";
import ReactTable from "react-table";
import { SelectedNodeId } from "../../app/AppContext";

export interface ITestBenchPageProps {
  nId: SelectedNodeId;
}

export interface ITestBenchPageState {
  nId: SelectedNodeId;
  testBenchDump: TestbenchReport<string, number> | null;
}

export default class TestBenchPage extends React.Component<ITestBenchPageProps, ITestBenchPageState> {
  constructor(props: ITestBenchPageProps) {
    super(props);

    this.state = {
      nId: props.nId,
      testBenchDump: null,
    };
    this.updateTestBench(this.state.nId);
  }

  componentWillReceiveProps(props: ITestBenchPageProps) {
    if (this.state.nId !== props.nId) {
      this.setState({ nId: props.nId });
      this.updateTestBench(this.state.nId);
    }
  }

  updateTestBench(nid: any) {
    haskellApiService
      .runTestBench(nid, "web_ui")
      .then((response: any) => {
        this.setState({
          testBenchDump: response.data,
        });
      })
      .catch((err: any) => {});
  }

  render() {
    return (
      <div>
        {this.state.testBenchDump === null && (
          <div className="m-3 text-black-50">
            <h5>Empty test bench</h5>
          </div>
        )}
        {this.state.testBenchDump != null && (
          <div className="mr-3">
            Status: <pre> {JSON.stringify(this.state.testBenchDump!.tbStatus)} </pre>
            <hr />
            <h3>Compiler output:</h3>
            <pre className="squeeze">
              {this.state.testBenchDump!.tbCompilerDump.map((e: string, i: number) => (
                <div key={i}>
                  {e}
                  <br />
                </div>
              ))}
            </pre>
            <hr />
            <h3>Simulation output:</h3>
            <pre className="squeeze">
              {this.state.testBenchDump!.tbSimulationDump.map((e: string, i: number) => (
                <div key={i}>
                  {e}
                  <br />
                </div>
              ))}
            </pre>
            <h3>Data:</h3>
            {this.renderSimulationData(
              this.state.testBenchDump!.tbFunctionalSimulationCntx,
              this.state.testBenchDump!.tbLogicalSimulationCntx
            )}
          </div>
        )}
      </div>
    );
  }

  renderSimulationData(functional: { [k: string]: number }[], logical: { [k: string]: number }[]) {
    let cntxs: Record<string, string>[] = [];
    for (let i = 0; i < functional.length; i++) {
      const funSim = functional[i];
      const logSim = logical[i];
      let cntx: Record<string, string> = { i: i.toString() };
      for (let key in logSim) {
        cntx[key] = funSim[key] === logSim[key] ? logSim[key].toString() : funSim[key] + " != " + logSim[key];
      }
      cntxs.push(cntx);
    }
    let columns: { Header: string; accessor: string }[] = [{ Header: "Cycle", accessor: "i" }];
    for (let key in logical[0]) {
      columns.push({ Header: key, accessor: key });
    }
    return (
      <div>
        <ReactTable
          defaultPageSize={functional.length}
          minRows={functional.length}
          showPagination={false}
          columns={columns}
          data={cntxs}
        />
        <pre>function simulation [ != logical simulation ]</pre>
      </div>
    );
  }
}
