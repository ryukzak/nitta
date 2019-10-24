import * as React from "react";
import "react-table/react-table.css";
import { haskellApiService } from "../../../services/HaskellApiService";
import { Graphviz } from "graphviz-react";
import { IGraphStructure, IGraphEdge } from "../../../gen/types";
import { SelectedNodeId } from "../../app/AppContext";

/**
 * Component to display algorithm graph.
 * Takes two arguments:
 * selectedNID - the node id that was selected;
 * view - the current view of program that determines the operation of the graph.
 * (Takes two kinds of view: "edges" or "synthesisNode")
 */

interface JsonResponse {
  [key: string]: any;
}

interface Graph {
  edges: JsonResponse;
  nodes: JsonResponse;
}

export interface IIntermediateViewProps {
  selectedNId: SelectedNodeId;
  view: string;
}

export interface IIntermediateViewState {
  selectedNId: SelectedNodeId;
  // FIXME: throw away useless property
  view: string;
  status: boolean;
  algGraph: IGraphStructure<IGraphEdge>;
}

export type IGraphJson = IGraphStructure<IGraphEdge>;

export class IntermediateView extends React.Component<IIntermediateViewProps, IIntermediateViewState> {
  constructor(props: IIntermediateViewProps) {
    super(props);
    this.state = {
      selectedNId: props.selectedNId,
      view: props.view,
      status: false,

      algGraph: {
        nodes: [],
        edges: [],
      },
    };

    this.graphMaker(props.selectedNId);
  }

  componentWillReceiveProps(props: IIntermediateViewProps) {
    if (this.state.selectedNId !== props.selectedNId) {
      this.setState({
        status: false,
        view: props.view,
      });
      this.graphMaker(props.selectedNId);
    }
  }

  graphMaker(nid: SelectedNodeId) {
    haskellApiService
      .simpleSynthesisGraph(nid)
      .then((response: any) => {
        // TODO: Replace with backend types
        let newNid = response.data;
        let graph: IGraphStructure<IGraphEdge> = { nodes: [], edges: [] };

        newNid.nodes.map((anObjectMapped: any, index: number) => {
          return (graph.nodes[index] = {
            id: index + 1,
            label: String(anObjectMapped.label),
            nodeColor: "",
            nodeShape: "",
            fontSize: "",
            nodeSize: "",
          });
        });
        newNid.edges.map((edge: any, index: number) => {
          return (graph.edges[index] = edge);
        });

        if (this.state.view === "synthesisNode") {
          this.setState({
            status: true,
            algGraph: graph,
          });
        }
      })
      .catch((err: any) => alert(err));
  }

  render() {
    return (
      <div className="justify-content-center bg-light border p-2 mx-auto">
        {this.state.status && (
          <Graphviz dot={renderGraphJsonToDot(this.state.algGraph)} options={{ height: 399, zoom: true }} />
        )}
      </div>
    );
  }
}

function isString(obj: any) {
  return typeof obj === "string" || obj instanceof String;
}

function renderDotOptions(options: JsonResponse) {
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
