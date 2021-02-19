import React, { useState, useEffect, useContext } from "react";
import { AxiosResponse, AxiosError } from "axios";

import { api, TestBenchReport } from "services/HaskellApiService";

import { AppContext, IAppContext } from "components/app/AppContext";
import { SimulationDataView } from "./SimulationDataView";

export const TestBenchPage: React.FC = () => {
  const appContext = useContext(AppContext) as IAppContext;

  const [requestSuccess, setRequestSuccess] = useState<boolean | null>(null);
  const [testBenchDump, setTestBenchDump] = useState<TestBenchReport | null>(null);

  useEffect(() => {
    api
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
    <div className="m-3">
      <h3>Status:</h3>
      <pre> {JSON.stringify(testBenchDump!.tbStatus)} </pre>
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
      <hr />
      <h3>Simulation data:</h3>
      <SimulationDataView
        functional={testBenchDump!.tbFunctionalSimulationCntx}
        logical={testBenchDump!.tbLogicalSimulationCntx}
      />
    </div>
  );
};
