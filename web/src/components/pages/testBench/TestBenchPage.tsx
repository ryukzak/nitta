import React, { useState, useEffect } from "react";
import { haskellApiService } from "../../../services/HaskellApiService";
import { TestbenchReport } from "../../../gen/types";
import { AxiosResponse, AxiosError } from "axios";
import { AppContext, IAppContext } from "../../app/AppContext";
import { useContext } from "react";
import { SimulationDataView } from "./SimulationDataView";

// FIXME: review, refactor

export const TestBenchPage: React.FC = () => {
  const appContext = useContext(AppContext) as IAppContext;

  const [testBenchDump, setTestBenchDump] = useState<TestbenchReport<string, number> | null>(null);

  useEffect(() => {
    haskellApiService
      .runTestBench(appContext.selectedNodeId, "web_ui")
      .then((response: AxiosResponse<TestbenchReport<string, number> | null>) => {
        setTestBenchDump(response.data);
      })
      .catch((err: AxiosError) => {});
  }, [appContext.selectedNodeId]);

  if (testBenchDump === null) {
    return (
      <div className="m-3 text-black-50">
        <h5>Empty test bench</h5>
      </div>
    );
  }

  return (
    <div className="mr-3">
      Status: <pre> {JSON.stringify(testBenchDump!.tbStatus)} </pre>
      <hr />
      <h3>Compiler output:</h3>
      <pre className="squeeze">
        {testBenchDump!.tbCompilerDump.map((e: string, i: number) => (
          <div key={i}>
            {e}
            <br />
          </div>
        ))}
      </pre>
      <hr />
      <h3>Simulation output:</h3>
      <pre className="squeeze">
        {testBenchDump!.tbSimulationDump.map((e: string, i: number) => (
          <div key={i}>
            {e}
            <br />
          </div>
        ))}
      </pre>
      <h3>Data:</h3>
      <SimulationDataView
        functional={testBenchDump!.tbFunctionalSimulationCntx}
        logical={testBenchDump!.tbLogicalSimulationCntx}
      />
    </div>
  );
};
