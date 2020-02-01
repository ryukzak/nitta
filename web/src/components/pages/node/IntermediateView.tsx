import * as React from "react";
import "react-table/react-table.css";
import { haskellApiService, UnitEndpoints } from "../../../services/HaskellApiService";
import { Graphviz } from "graphviz-react";
import { IGraphStructure, IGraphEdge, INodeElement } from "../../../gen/types";
import { AppContext, IAppContext } from "../../app/AppContext";
import { AxiosResponse, AxiosError } from "axios";
import { NodeView, GraphStructure, NodeElement, GraphEdge } from "../../../gen/types";

import "./IntermediateView.scss";

/**
 * Component to display algorithm graph.
 */

export interface IIntermediateViewProps {}

export type IGraphJson = IGraphStructure<IGraphEdge>;
type Node = NodeView<string, string, string, string>;

interface ProcessState {
  bindeFuns: string[];
  transferedVars: string[];
}

interface EndpointStatus {
  sources: string[];
  targets: string[];
}

export const IntermediateView: React.FC<IIntermediateViewProps> = props => {
  const { selectedNodeId } = React.useContext(AppContext) as IAppContext;

  const [algorithmGraph, setAlgorithmGraph] = React.useState<IGraphJson | null>(null);
  const [varStatus, setVarStatus] = React.useState<ProcessState>({ bindeFuns: [], transferedVars: [] });
  const [endpointSt, setEndpointSt] = React.useState<EndpointStatus>({ sources: [], targets: [] });

  // Updating graph
  React.useEffect(() => {
    haskellApiService
      .simpleSynthesisGraph(selectedNodeId)
      .then((response: AxiosResponse<GraphStructure<GraphEdge>>) => {
        const graphData = response.data;
        const newGraph: IGraphJson = {
          nodes: graphData.nodes.map((nodeData: NodeElement, index: number) => {
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
          edges: graphData.edges.map((edgeData: GraphEdge, index: number) => {
            return edgeData;
          })
        };

        setAlgorithmGraph(newGraph);
      })
      .catch((err: any) => console.error(err));

    haskellApiService
      .getPath(selectedNodeId)
      .then((response: AxiosResponse<Node[]>) => {
        let result: ProcessState = { bindeFuns: [], transferedVars: [] };
        response.data.forEach((n: Node) => {
          if (n.nvOrigin !== null && n.nvOrigin!.decision.tag === "DataflowView") {
            let targets = n.nvOrigin!.decision.targets;
            Object.keys(targets).forEach((v: string) => {
              if (targets[v] !== null) result.transferedVars.push(v);
            });
          }
          if (n.nvOrigin !== null && n.nvOrigin!.decision.tag === "BindingView") {
            result.bindeFuns.push(n.nvOrigin!.decision.function.fvFun, ...n.nvOrigin!.decision.function.fvHistory);
          }
        });
        setVarStatus(result);
      })
      .catch((err: AxiosError) => console.log(err));

    haskellApiService
      .getEndpoints(selectedNodeId)
      .then((response: AxiosResponse<UnitEndpoints>) => {
        let result: EndpointStatus = { sources: [], targets: [] };
        response.data.forEach(e => {
          let role = e.endpoints.epRole;
          if (role.tag === "Source") {
            result.sources.push(...role.contents);
          }
          if (role.tag === "Target") {
            result.targets.push(role.contents);
          }
        });
        setEndpointSt(result);
      })
      .catch((err: AxiosError) => console.log(err));
  }, [selectedNodeId]);

  return (
    <div className="bg-light border edgeGraphContainer">
      {algorithmGraph && (
        <Graphviz
          dot={renderGraphJsonToDot(algorithmGraph, varStatus, endpointSt)}
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

function isFunctionBinded(binded: string[], node: INodeElement): boolean {
  if (binded.indexOf(node.function) >= 0) {
    return true;
  }
  for (let e of node.history) {
    if (binded.indexOf(e) >= 0) return true;
  }
  return false;
}

function renderGraphJsonToDot(json: IGraphJson, state: ProcessState, endpoints: EndpointStatus): string {
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
        style: isTransfered(edge.label) ? "line" : "dashed"
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
