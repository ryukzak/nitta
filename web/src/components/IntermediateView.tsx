import * as React from "react";
import "react-table/react-table.css";
import { haskellAPI } from "../middleware/haskell-api";
import { Graphviz } from "graphviz-react";
import { IGraphStructure, IGraphEdge } from "../gen/types";

/**
 * Component to display algorithm graph.
 * Takes two arguments:
 * selectedNID - the node id that was selected;
 * view - the current view of program that determines the operation of the graph.
 * (Takes two kinds of view: "edges" or "synthesisNode")
 */

interface IntermediateViewProps {
  selectedNId: string;
  view: string;
}

interface IntermediateViewState {
  selectedNId: string;
  view: string;
  status: boolean;
  graph: any;
}


export type IGraphJson = IGraphStructure<IGraphEdge>;

export class IntermediateView extends React.Component<IntermediateViewProps, IntermediateViewState> {
  constructor(props: IntermediateViewProps) {
    super(props);
    this.state = {
      selectedNId: props.selectedNId,
      view: props.view,
      status: false,

      graph: {
        nodes: [],
        edges: []
      }
    };

    this.graphMaker(props.selectedNId);
  }


  componentWillReceiveProps(props: IntermediateViewProps) {
    if (this.state.selectedNId !== props.selectedNId) {
      this.setState({
        status: false,
        view: props.view
      });
      this.graphMaker(props.selectedNId);
    }
  }

  graphMaker(nid: string) {
    haskellAPI.simpleSynthesisGraph(nid)
      .then((response: any) => {
        // TODO: Replace with backend types
        let newNid = response.data;
        let graph = { nodes: [], edges: [] };

        newNid.nodes.map((anObjectMapped: any, index: number) => {
          graph.nodes[index] = { id: index + 1, label: String(anObjectMapped.label) };
        });
        newNid.edges.map((edge: any, index: number) => {
          graph.edges[index] = edge;
        });

        if (this.state.view === "synthesisNode") {
          this.setState({ status: true, graph });
        }
      })
      .catch((err: any) => alert(err));
  }

  render() {
    return (
      <>
        {this.state.status
          &&
          <Graphviz
            dot={renderGraphJsonToDot(this.state.graph)}
            options={{ height: 399, zoom: true }}
          />
        }
      </>
    );
  }
}

function isString(obj: any) {
  return typeof obj === "string" || obj instanceof String;
}

function renderDotOptions(options: object) {
  let result = [];

  for (let key in options) {
    let representation = (isString(options[key])) ? `"${options[key]}"` : options[key];
    result.push(`${key}=${representation}`);
  }

  return `[${result.join("; ")}]`;
}

function renderGraphJsonToDot(json: IGraphJson): string {
  let lines = [
    // "rankdir=LR"
  ];

  lines.push(...json.nodes.map(
    node => node.id + " " + renderDotOptions({ label: node.label })
  ));

  lines.push(...json.edges.map(
    edge => `${edge.from} -> ${edge.to} ` + renderDotOptions({ label: edge.label })
  ));

  const wrap = (content: string) => `\t${content};`;
  let result = `digraph {\n${lines.map(wrap).join("\n")}\n}`;
  console.log(result);
  return result;
}
