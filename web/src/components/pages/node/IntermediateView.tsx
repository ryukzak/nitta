import * as React from "react";
import "react-table/react-table.css";
import { Graphviz } from "graphviz-react";
import { AppContext, IAppContext } from "../../app/AppContext";
import { AxiosResponse, AxiosError } from "axios";
import { GraphNode, GraphEdge } from "../../../gen/types";
import {
  haskellApiService,
  UnitEndpoints,
  IntermediateGraph,
  SynthesisNode
} from "../../../services/HaskellApiService";

import "./IntermediateView.scss";

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

export const IntermediateView: React.FC<IIntermediateViewProps> = props => {
  const { selectedNodeId } = React.useContext(AppContext) as IAppContext;

  const [algorithmGraph, setAlgorithmGraph] = React.useState<IntermediateGraph | null>(null);
  const [procState, setProcState] = React.useState<ProcessState>({ bindeFuns: [], transferedVars: [] });
  const [endpoints, setEndpoints] = React.useState<Endpoints>({ sources: [], targets: [] });

  // Updating graph
  React.useEffect(() => {
    haskellApiService
      .getIntermediateView(selectedNodeId)
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
              nodeSize: ""
            };
          }),
          edges: graphData.edges.map((edgeData: GraphEdge) => {
            return edgeData;
          })
        };
        setAlgorithmGraph(newGraph);
      })
      .catch((err: AxiosError) => console.error(err));

    haskellApiService
      .getRootPath(selectedNodeId)
      .then((response: AxiosResponse<SynthesisNode[]>) => {
        let result: ProcessState = { bindeFuns: [], transferedVars: [] };
        response.data.forEach((n: SynthesisNode) => {
          if (n.nvOrigin !== null && n.nvOrigin!.decision.tag === "DataflowView") {
            let targets = n.nvOrigin!.decision.targets;
            Object.keys(targets).forEach((v: string) => {
              if (targets[v] !== null) result.transferedVars.push(v);
            });
          }
          if (n.nvOrigin !== null && n.nvOrigin!.decision.tag === "BindingView") {
            let d = n.nvOrigin!.decision;
            result.bindeFuns.push(d.function.fvFun, ...d.function.fvHistory);
          }
        });
        setProcState(result);
      })
      .catch((err: AxiosError) => console.log(err));

    haskellApiService
      .getEndpoints(selectedNodeId)
      .then((response: AxiosResponse<UnitEndpoints>) => {
        let result: Endpoints = { sources: [], targets: [] };
        response.data.forEach(e => {
          let role = e.endpoints.epRole;
          if (role.tag === "Source") {
            result.sources.push(...role.contents);
          }
          if (role.tag === "Target") {
            result.targets.push(role.contents);
          }
        });
        setEndpoints(result);
      })
      .catch((err: AxiosError) => console.log(err));
  }, [selectedNodeId]);

  return (
    <div className="bg-light border edgeGraphContainer">
      {algorithmGraph && (
        <Graphviz
          dot={renderGraphJsonToDot(algorithmGraph, procState, endpoints)}
          options={{ height: 399, width: "100%", zoom: true }}
        />
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

  let nodes: string[] = json.nodes.map(node => {
    return (
      node.id +
      " " +
      renderDotOptions({
        label: node.label,
        style: isFunctionBinded(state.bindeFuns, node) ? "line" : "dashed"
      })
    );
  });
  function isTransfered(v: string): boolean {
    return state.transferedVars.indexOf(v) >= 0;
  }
  let edges = json.edges.map(edge => {
    return (
      `${edge.from} -> ${edge.to} ` +
      renderDotOptions({
        label: edge.label,
        style: isTransfered(edge.label) ? "line" : "dashed",
        dir: "both",
        arrowhead: endpoints.targets.indexOf(edge.label) >= 0 || isTransfered(edge.label) ? "" : "o",
        arrowtail: endpoints.sources.indexOf(edge.label) >= 0 || isTransfered(edge.label) ? "dot" : "odot"
      })
    );
  });

  lines.push(...nodes);
  lines.push(...edges);

  const wrap = (content: string) => `\t${content};`;
  let result = `digraph {\n${lines.map(wrap).join("\n")}\n}`;
  return result;
}
