import React, { useState, useEffect, useContext, FC } from "react";
import { AxiosResponse, AxiosError } from "axios";

import { api, TestBenchReportData } from "services/HaskellApiService";

import { AppContext, IAppContext } from "app/AppContext";
import { TestBenchSimulationLog } from "components/TestBenchSimulationLog";

export const TestBenchScreen: FC = () => {
  const appContext = useContext(AppContext) as IAppContext;

  const [requestSuccess, setRequestSuccess] = useState<boolean | null>(null);
  const [testBenchDump, setTestBenchDump] = useState<TestBenchReportData | null>(null);

  useEffect(() => {
    api
      .runTestBench(appContext.selectedSID, "web_ui", 5)
      .then((response: AxiosResponse<TestBenchReportData | null>) => {
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
      <pre className="squeeze">{testBenchDump!.tbCompilerDump}</pre>
      <hr />
      <h3>Simulation output:</h3>
      <pre className="squeeze">{testBenchDump!.tbSimulationDump}</pre>
      <hr />
      <h3>Simulation data:</h3>
      <TestBenchSimulationLog
        functional={testBenchDump!.tbFunctionalSimulationLog}
        logical={testBenchDump!.tbLogicalSimulationLog}
      />
    </div>
  );
};
