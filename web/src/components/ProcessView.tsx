import Axios, { AxiosResponse, AxiosError } from "axios";
import React, { useContext, useState, useEffect, FC } from "react";
import "react-table/react-table.css";

import { AppContext, IAppContext } from "app/AppContext";
import { api, ProcessData, StepData, RelationData } from "services/HaskellApiService";

import "components/Graphviz.scss";

import dynamic from "next/dynamic";
import axiosErrorExceptionHandler from "./utils/axios_errors_handlers/AxiosErrorHander";

const Graphviz = dynamic(() => import("../components/Graphviz"), { ssr: false });
/**
 * Component to display target process by GraphViz.
 */

export interface IProcessViewProps {}

export const ProcessView: FC<IProcessViewProps> = (props) => {
  const { selectedSID } = useContext(AppContext) as IAppContext;

  const [process, setProcess] = useState<ProcessData | null>(null);

  useEffect(() => {
    const source = Axios.CancelToken.source();

    api
      .getProcess(selectedSID, source.token)
      .then((response: AxiosResponse<ProcessData>) => setProcess(response.data))
      .catch((err: AxiosError) => {
        axiosErrorExceptionHandler(err);
      });

    return () => {
      source.cancel();
    };
  }, [selectedSID]);

  if (!process) {
    return <pre>LOADING...</pre>;
  }
  if (process.steps.length === 0) {
    return <pre>Process is empty.</pre>;
  }
  return (
    <div className="bg-light border graphvizContainer">
      {process && <Graphviz dot={renderProcessViewDot(process)} options={{ height: 399, width: "100%", zoom: true }} />}
    </div>
  );
};

function renderProcessViewDot(process: ProcessData): string {
  var lines: string[] = [];

  lines.push("digraph {");
  lines.push("  rankdir=LR;");
  process.steps.forEach((step: StepData) => {
    var label = step.pDesc.replace(": ", ":\n");
    lines.push(`  ${step.pID}[label="${label}"];`);
  });

  process.relations.forEach((relation: RelationData) => {
    // FIXME: replace by commented code after source code update
    lines.push(`  ${relation[0]} -> ${relation[1]};`);
    /* if (relation.tag === "Vertical") {
     *   lines.push(`  ${relation.vUp} -> ${relation.vDown};`);
     * }
     * if (relation.tag === "Horizontal") {
     *   lines.push(`  ${relation.hPrev} -> ${relation.hNext}[style=dashed];`);
     * } */
  });
  lines.push("}");
  return lines.join("\n");
}
