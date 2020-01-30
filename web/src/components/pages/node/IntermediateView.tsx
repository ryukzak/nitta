import * as React from "react";
import "react-table/react-table.css";
import { haskellApiService } from "../../../services/HaskellApiService";
import { Graphviz } from "graphviz-react";
import { IGraphStructure, IGraphEdge } from "../../../gen/types";
import { AppContext, IAppContext } from "../../app/AppContext";
import { AxiosResponse, AxiosError } from "axios";
import { NodeView } from "../../../gen/types";

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

export const IntermediateView: React.FC<IIntermediateViewProps> = props => {
  const { selectedNodeId } = React.useContext(AppContext) as IAppContext;

  const [algorithmGraph, setAlgorithmGraph] = React.useState<IGraphJson | null>(null);
  const [varStatus, setVarStatus] = React.useState<ProcessState>({ bindeFuns: [], transferedVars: [] });

  // Updating graph
  React.useEffect(() => {
    haskellApiService
      .simpleSynthesisGraph(selectedNodeId)
      .then((response: any) => {
        const graphData = response.data;
        const newGraph: IGraphJson = {
          nodes: graphData.nodes.map((nodeData: any, index: number) => {
            return {
              id: index + 1,
              label: String(nodeData.label),
              nodeColor: "",
              nodeShape: "",
              fontSize: "",
              nodeSize: ""
            };
          }),
          edges: graphData.edges.map((edgeData: any, index: number) => {
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
            Object.keys(n.nvOrigin!.decision.targets).forEach((v: string) => result.transferedVars.push(v));
          }
          if (n.nvOrigin !== null && n.nvOrigin!.decision.tag === "BindingView") {
            result.bindeFuns.push(n.nvOrigin!.decision.function);
          }
        });
        setVarStatus(result);
      })
      .catch((err: AxiosError) => console.log(err));
  }, [selectedNodeId]);

  return (
    <div className="bg-light border edgeGraphContainer">
      {algorithmGraph && (
        <Graphviz
          dot={renderGraphJsonToDot(algorithmGraph, varStatus)}
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

function renderGraphJsonToDot(json: IGraphJson, state: ProcessState): string {
  let lines = [
    // "rankdir=LR"
  ];

  let nodes: string[] = json.nodes.map(node => {
    return (
      node.id +
      " " +
      // FIXME: incorrect work with function after Refactoring, should be fixed on
      renderDotOptions({
        label: node.label,
        style: state.bindeFuns.indexOf(node.function) >= 0 ? "line" : "dashed"
      })
    );
  });
  let edges = json.edges.map(edge => {
    return (
      `${edge.from} -> ${edge.to} ` +
      renderDotOptions({
        label: edge.label,
        style: state.transferedVars.indexOf(edge.label) >= 0 ? "line" : "dashed"
      })
    );
  });

  lines.push(...nodes);
  lines.push(...edges);

  const wrap = (content: string) => `\t${content};`;
  let result = `digraph {\n${lines.map(wrap).join("\n")}\n}`;
  return result;
}
