import * as React from "react";
import ReactTable from "react-table";

export interface ITestBenchSimulationLogProps {
  functional: { [k: string]: number }[];
  logical: { [k: string]: number }[];
}

export const TestBenchSimulationLog: React.FC<ITestBenchSimulationLogProps> = ({ functional, logical }) => {
  let columns: { Header: string; accessor: string }[] = [{ Header: "Cycle", accessor: "i" }];
  for (let key in logical[0]) {
    columns.push({ Header: key, accessor: key });
  }

  let cntxs: Record<string, string>[] = [];
  for (let i = 0; i < functional.length; i++) {
    const funSim = functional[i];
    const logSim = logical[i];
    let cntx: Record<string, string> = { i: i.toString() };
    for (let key in logSim) {
      if (funSim[key] === logSim[key]) cntx[key] = logSim[key].toString();
      else cntx[key] = funSim[key] + " != " + logSim[key];
    }
    cntxs.push(cntx);
  }

  return (
    <>
      <ReactTable
        defaultPageSize={functional.length}
        minRows={functional.length}
        showPagination={false}
        columns={columns}
        data={cntxs}
      />
      <pre>function simulation [ != logical simulation ]</pre>
    </>
  );
};
