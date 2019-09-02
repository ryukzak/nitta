import * as React from "react";
import "react-table/react-table.css";
import { haskellAPI } from "../middleware/haskell-api";
import { Graphviz } from "graphviz-react";
import { renderGraphJsonToDot } from "./GraphUtils";

/**
 * Component to display algorithm graph.
 * Takes two arguments:
 * selectedNID - the node id that was selected;
 * view - the current view of program that determines the operation of the graph.
 * (Takes two kinds of view: "edges" or "synthesisNode")
 */

interface GraphViewProps {
  selectedNId: number;
  view: string;
}

interface GraphViewState {
  selectedNId: number;
  view: string;
  status: boolean;
  events: any;
  options: any;
  graph: any;
}

export class GraphView extends React.Component<GraphViewProps, GraphViewState> {
  constructor(props: GraphViewProps) {
    super(props);
    this.state = {
      selectedNId: props.selectedNId,
      view: props.view,
      status: false,

      events: {
        select: function (event: any) {
          let { nodes, edges } = event;
        }
      },

      options: {
        layout: { hierarchical: false },
        edges: { color: "#000000" },
        nodes: {},
        physics: { enabled: false }
      },

      graph: {
        nodes: [],
        edges: []
      }
    };

    this.graphMaker(props.selectedNId);
  }


  componentWillReceiveProps(props: GraphViewProps) {
    if (this.state.selectedNId !== props.selectedNId) {
      this.setState({
        status: false,
        view: props.view
      });
      this.graphMaker(props.selectedNId);
    }
  }

  graphMaker(nid: number) {
    haskellAPI.simpleSynthesisGraph(nid)
      .then((response: any) => {
        let newNid = response.data;
        let graph = { nodes: [], edges: [] };

        newNid.nodes.map((anObjectMapped: any, index: number) => {
          graph.nodes[index] = { id: index + 1, label: String(anObjectMapped.label) };
        });
        newNid.edges.map((anObjectMapped: any, index: number) => {
          graph.edges[index] = ({ from: anObjectMapped.from, to: anObjectMapped.to, label: anObjectMapped.label });
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
