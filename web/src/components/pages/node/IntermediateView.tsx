import * as React from "react";
import "react-table/react-table.css";
import { haskellApiService } from "../../../services/HaskellApiService";
import { Graphviz } from "graphviz-react";
import { IGraphStructure, IGraphEdge } from "../../../gen/types";
import { AppContext, IAppContext } from "../../app/AppContext";

import "./IntermediateView.scss";

/**
 * Component to display algorithm graph.
 */

export interface IIntermediateViewProps {}

export type IGraphJson = IGraphStructure<IGraphEdge>;

export const IntermediateView: React.FC<IIntermediateViewProps> = props => {
  const { selectedNodeId } = React.useContext(AppContext) as IAppContext;

  const [algorithmGraph, setAlgorithmGraph] = React.useState<IGraphJson | null>(null);

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
              nodeSize: "",
            };
          }),
          edges: graphData.edges.map((edgeData: any, index: number) => {
            return edgeData;
          }),
        };

        setAlgorithmGraph(newGraph);
      })
      .catch((err: any) => console.error(err));
  }, [selectedNodeId]);

  return (
    <div className="bg-light border edgeGraphContainer">
      {algorithmGraph && <Graphviz dot={renderGraphJsonToDot(algorithmGraph)} options={{ height: 399, width: "100%", zoom: true }} />}
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

function renderGraphJsonToDot(json: IGraphJson): string {
  let lines = [
    // "rankdir=LR"
  ];

  lines.push(...json.nodes.map(node => node.id + " " + renderDotOptions({ label: node.label })));

  lines.push(...json.edges.map(edge => `${edge.from} -> ${edge.to} ` + renderDotOptions({ label: edge.label })));

  const wrap = (content: string) => `\t${content};`;
  let result = `digraph {\n${lines.map(wrap).join("\n")}\n}`;
  return result;
}
