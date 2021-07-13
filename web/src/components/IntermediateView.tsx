import { AxiosResponse, AxiosError } from "axios";
import React, { useContext, useState, useEffect, FC } from "react";
import "react-table/react-table.css";
import { Graphviz } from "graphviz-react";

import { AppContext, IAppContext } from "app/AppContext";
import { GraphNode, GraphEdge } from "services/gen/types";
import { api, IntermediateGraph, Dataflow, Bind, Node } from "services/HaskellApiService";
import { UnitEndpointsData, EndpointOptionData, EndpointDecision } from "services/HaskellApiService";
import { DownloadTextFile } from "utils/download";

import "components/Graphviz.scss";

/**
 * Component to display algorithm graph.
 */

export interface IIntermediateViewProps {}

interface ProcessState {
  bindeFuns: string[];
  transferedVars: string[];
}

interface Endpoints {
  sources: string[];
  targets: string[];
}

export const IntermediateView: FC<IIntermediateViewProps> = (props) => {
  const { selectedSID } = useContext(AppContext) as IAppContext;

  const [algorithmGraph, setAlgorithmGraph] = useState<IntermediateGraph | null>(null);
  const [procState, setProcState] = useState<ProcessState>({ bindeFuns: [], transferedVars: [] });
  const [endpoints, setEndpoints] = useState<Endpoints>({ sources: [], targets: [] });

  // Updating graph
  useEffect(() => {
    api
      .getIntermediateView(selectedSID)
      .then((response: AxiosResponse<IntermediateGraph>) => {
        const graphData = response.data;
        const newGraph: IntermediateGraph = {
          nodes: graphData.nodes.map((nodeData: GraphNode, index: number) => {
            return {
              id: index + 1,
              label: String(nodeData.label),
              function: nodeData.function,
              history: nodeData.history,
              nodeColor: "",
              nodeShape: "",
              fontSize: "",
              nodeSize: "",
            };
          }),
          edges: graphData.edges.map((edgeData: GraphEdge) => {
            return edgeData;
          }),
        };
        setAlgorithmGraph(newGraph);
      })
      .catch((err: AxiosError) => console.error(err));

    api
      .getRootPath(selectedSID)
      .then((response: AxiosResponse<Node[]>) => {
        let result: ProcessState = { bindeFuns: [], transferedVars: [] };
        response.data.forEach((n: Node) => {
          if (n.decision.tag === "DataflowDecisionView") {
            let targets = (n.decision as Dataflow).targets;
            targets.forEach((target: [string, EndpointDecision]) => {
              result.transferedVars.push(target[1].epRole.contents as string);
            });
          }
          if (n.decision.tag === "BindDecisionView") {
            let d = n.decision as Bind;
            result.bindeFuns.push(d.function.fvFun, ...d.function.fvHistory);
          }
        });
        setProcState(result);
      })
      .catch((err: AxiosError) => console.log(err));

    api
      .getEndpoints(selectedSID)
      .then((response: AxiosResponse<UnitEndpointsData[]>) => {
        let result: Endpoints = { sources: [], targets: [] };
        response.data.forEach((eps: UnitEndpointsData) => {
          eps.unitEndpoints.forEach((e: EndpointOptionData) => {
            let role = e.epRole;
            if (role.tag === "Source") {
              result.sources.push(...role.contents);
            }
            if (role.tag === "Target") {
              result.targets.push(role.contents);
            }
          });
        });
        setEndpoints(result);
      })
      .catch((err: AxiosError) => console.log(err));
  }, [selectedSID]);

  // TODO: is renderGraphJsonToDot expensive? may be a good idea to wrap expression in useMemo, otherwise it's called on
  // each rerender
  const dot = algorithmGraph ? renderGraphJsonToDot(algorithmGraph, procState, endpoints) : undefined;
  return (
    <div className="bg-light border graphvizContainer">
      {dot && (
        <>
          <Graphviz dot={dot} options={{ height: 399, width: "100%", zoom: true }} />
          <DownloadTextFile name={"algorithm.dot"} text={dot} />
        </>
      )}
    </div>
  );
};

interface DotOptions {
  [key: string]: any;
}

function isString(obj: any) {
  return typeof obj === "string" || obj instanceof String;
}

function renderDotOptions(options: DotOptions) {
  let result = [];
  let key: string;
  for (key in options) {
    let representation: string = isString(options[key]) ? `"${options[key]}"` : options[key];
    result.push(`${key}=${representation}`);
  }
  return `[${result.join("; ")}]`;
}

function isFunctionBinded(binded: string[], node: GraphNode): boolean {
  if (binded.indexOf(node.function) >= 0) {
    return true;
  }
  for (let e of node.history) {
    if (binded.indexOf(e) >= 0) return true;
  }
  return false;
}

function renderGraphJsonToDot(json: IntermediateGraph, state: ProcessState, endpoints: Endpoints): string {
  let lines = [
    // "rankdir=LR"
  ];

  let nodes: string[] = json.nodes.map((node) => {
    return (
      node.id +
      " " +
      renderDotOptions({
        label: node.label,
        style: isFunctionBinded(state.bindeFuns, node) ? "line" : "dashed",
      })
    );
  });
  function isTransfered(v: string): boolean {
    return state.transferedVars.indexOf(v) >= 0;
  }
  let edges = json.edges.map((edge) => {
    return (
      `${edge.from} -> ${edge.to} ` +
      renderDotOptions({
        label: edge.label,
        style: isTransfered(edge.label) ? "line" : "dashed",
        dir: "both",
        arrowhead: endpoints.targets.indexOf(edge.label) >= 0 || isTransfered(edge.label) ? "" : "o",
        arrowtail: endpoints.sources.indexOf(edge.label) >= 0 || isTransfered(edge.label) ? "dot" : "odot",
      })
    );
  });

  lines.push(...nodes);
  lines.push(...edges);

  const wrap = (content: string) => `\t${content};`;
  let result = `digraph {\n${lines.map(wrap).join("\n")}\n}`;
  return result;
}
