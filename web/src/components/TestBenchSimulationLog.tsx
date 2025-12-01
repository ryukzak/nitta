import React, { FC } from "react";
import {
  useReactTable,
  getCoreRowModel,
  flexRender,
  ColumnDef,
} from "@tanstack/react-table";

export interface ITestBenchSimulationLogProps {
  functional: { [k: string]: number }[];
  logical: { [k: string]: number }[];
}

export const TestBenchSimulationLog: FC<ITestBenchSimulationLogProps> = ({ functional, logical }) => {

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

  // Build columns
  const columns: ColumnDef<Record<string, string>>[] = [
    {
      header: "Cycle",
      accessorKey: "i",
    },
    ...Object.keys(logical[0]).map((key) => ({
      header: key,
      accessorKey: key,
    })),
  ];

  const table = useReactTable({
    data: cntxs,
    columns,
    getCoreRowModel: getCoreRowModel(),
  });

  return (
    <>
      <table>
        <thead>
          {table.getHeaderGroups().map((hg) => (
            <tr key={hg.id}>
              {hg.headers.map((h) => (
                <th key={h.id}>
                  {flexRender(h.column.columnDef.header, h.getContext())}
                </th>
              ))}
            </tr>
          ))}
        </thead>

        <tbody>
          {table.getRowModel().rows.map((row) => (
            <tr key={row.id}>
              {row.getVisibleCells().map((cell) => (
                <td key={cell.id}>
                  {flexRender(cell.column.columnDef.cell, cell.getContext())}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>

      <pre>function simulation [ != logical simulation ]</pre>
    </>
  );
};
