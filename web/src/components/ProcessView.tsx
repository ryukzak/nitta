import { AxiosError, AxiosResponse } from "axios";
import { Graphviz } from "graphviz-react";
import React, { FC, useContext, useEffect, useState } from "react";
import "react-table/react-table.css";

import { AppContext, IAppContext } from "app/AppContext";
import { ProcessData, RelationData, StepData, api } from "services/HaskellApiService";

import "components/Graphviz.scss";

/**
 * Component to display target process by GraphViz.
 */

export interface IProcessViewProps {}

export const ProcessView: FC<IProcessViewProps> = (props) => {
  const { selectedSid } = useContext(AppContext) as IAppContext;

  const [process, setProcess] = useState<ProcessData | null>(null);

  useEffect(() => {
    api
      .getProcess(selectedSid)
      .then((response: AxiosResponse<ProcessData>) => setProcess(response.data))
      .catch((err: AxiosError) => console.error(err));
  }, [selectedSid]);

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
    if (relation.tag === "Vertical") {
      lines.push(`  ${relation.vUp} -> ${relation.vDown};`);
    }
    if (relation.tag === "Horizontal") {
      lines.push(`  ${relation.hPrev} -> ${relation.hNext}[style=dashed];`);
    }
  });
  lines.push("}");
  return lines.join("\n");
}
