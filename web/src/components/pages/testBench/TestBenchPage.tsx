import React, { useState, useEffect } from "react";
import { haskellApiService, TestBenchReport } from "services/HaskellApiService";
import { TestbenchReportView } from "gen/types";
import { AxiosResponse, AxiosError } from "axios";
import { AppContext, IAppContext } from "components/app/AppContext";
import { useContext } from "react";
import { SimulationDataView } from "./SimulationDataView";

// FIXME: review, refactor

export const TestBenchPage: React.FC = () => {
  const appContext = useContext(AppContext) as IAppContext;

  const [requestSuccess, setRequestSuccess] = useState<boolean | null>(null);
  const [testBenchDump, setTestBenchDump] = useState<TestbenchReportView<string, number> | null>(null);

  useEffect(() => {
    haskellApiService
      .runTestBench(appContext.selectedSID, "web_ui", 5)
      .then((response: AxiosResponse<TestBenchReport | null>) => {
        setTestBenchDump(response.data);
        setRequestSuccess(true);
      })
      .catch((err: AxiosError) => {
        setRequestSuccess(false);
      });
  }, [appContext.selectedSID]);

  if (requestSuccess === null) {
    return (
      <div className="m-3 text-black-50">
        <h5>Loading...</h5>
      </div>
    );
  }

  if (requestSuccess === false) {
    return (
      <div className="m-3 text-black-50">
        <h5>Can not get testbench data for the not finished synthesis.</h5>
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
