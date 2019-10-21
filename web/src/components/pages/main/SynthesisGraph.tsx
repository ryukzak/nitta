import * as React from "react";
import Tree from "react-d3-tree";
import { haskellApiService } from "../../../services/HaskellApiService";

interface JsonResponse {
  [key: string]: any;
}

interface JsonObjId {
  [key: string]: graph;
}

interface graph {
  name?: string;
  nid?: number;
  attributes?: JsonResponse;
  status?: string;
  children?: graph[];
  nodeSvgShape?: any;
  nodeSvgShapeOriginal?: any;
}

interface SynthesisGraphProps {
  onNIdChange: any;
  selectedNId: any;
}

interface SynthesisGraphState {
  selectedNId: string | null;
  dataGraph: Array<any>;
  nIds: JsonObjId;
}

export default class SynthesisGraph extends React.Component<SynthesisGraphProps, SynthesisGraphState> {
  onNIdChange: any;

  constructor(props: SynthesisGraphProps) {
    super(props);
    this.onNIdChange = props.onNIdChange;
    this.state = {
      selectedNId: props.selectedNId,
      dataGraph: [],
      nIds: {}
    };
    this.reloadSynthesisGraph();
  }

  componentWillReceiveProps(newProps: SynthesisGraphProps) {
    console.debug(
      "SynthesisGraph:componentWillReceiveProps() // props.selectedNId, this.state.selectedNId:",
      newProps.selectedNId,
      this.state.selectedNId
    );

    if (newProps.selectedNId !== null && !(newProps.selectedNId in this.state.nIds)) {
      this.setState({
        selectedNId: newProps.selectedNId
      });
      this.reloadSynthesisGraph();
      return;
    }
    if (newProps.selectedNId !== null && this.state.selectedNId !== newProps.selectedNId) {
      this.unmarkNode(this.state.selectedNId);
      this.markNode(newProps.selectedNId);
      this.setState({
        selectedNId: newProps.selectedNId,
        dataGraph: [this.state.dataGraph[0]] // force re-render Tree
      });
    }
    if (newProps.selectedNId === null) {
      this.setState({
        dataGraph: [],
        nIds: {}
      });
      this.onNIdChange("-");
    }
  }

  markNode(nid: any, nIds?: any, color?: any) {
    if (color === undefined) color = "blue";
    if (nIds === undefined) nIds = this.state.nIds;
    if (nid === null || nIds === null) return;

    if (color === "blue") {
      nIds[nid].nodeSvgShapeOriginal = nIds[nid].nodeSvgShape;
    }
    console.debug("SynthesisGraph:markNode(", nid, nIds, color, ")");
    nIds[nid].nodeSvgShape = {
      shape: "circle",
      shapeProps: {
        r: 10,
        cx: 0,
        cy: 0,
        fill: color
      }
    };
  }

  unmarkNode(nid: any) {
    console.debug("SynthesisGraph:unmarkNode(", nid, ")");
    if (nid === null) return;
    let tmp: string = this.state.nIds[nid].nodeSvgShapeOriginal;
    let nids = this.state.nIds;
    nids[nid].nodeSvgShape = tmp;
    this.setState({
      nIds: nids
    });
  }

  reloadSynthesisGraph = () => {
    console.debug("SynthesisGraph:reloadSynthesisGraph()");
    let reLastNidStep = /-[^-]*$/; // nInSeparator
    let nid = this.state.selectedNId;
    haskellApiService
      .getSynthesis()
      .then((response: any) => {
        let nIds: JsonObjId = {};
        let buildGraph = (gNode: graph, dNode: JsonResponse) => {
          gNode.name = reLastNidStep.exec(dNode[0].svNnid)![0];
          gNode.nid = dNode[0].svNnid;
          nIds[dNode[0].svNnid] = gNode;
          if (dNode[0].svIsEdgesProcessed) this.markNode(gNode.nid, nIds, "black");
          if (dNode[0].svIsComplete) this.markNode(gNode.nid, nIds, "lime");
          gNode.attributes = {
            dec: dNode[0].svOptionType,
            ch: dNode[0].svDuration + " / " + dNode[0].svCharacteristic
          };
          gNode.status = dNode[0].svIsComplete;
          dNode[0].svCntx.forEach((e: string, i: any) => {
            gNode.attributes![i] = e;
          });
          gNode.children = [];
          dNode[1].forEach((e: any) => {
            var tmp: graph = {};
            if (gNode.children != null) {
              gNode.children.push(tmp);
              buildGraph(tmp, e);
            }
          });
          return gNode;
        };
        let graph = buildGraph({}, response.data);
        nIds["."] = graph;
        if (nid !== null) this.markNode(nid, nIds);

        this.setState({
          dataGraph: [graph],
          nIds: nIds
        });
      })
      .catch((err: any) => console.log(err));
  };

  render() {
    if (this.state.dataGraph === null || this.state.dataGraph.length === 0)
      return (
        <div className="h-100 d-flex align-items-center justify-content-center text-black-50">
          <h1>Empty graph</h1>
        </div>
      );
    return (
      <div className="h-100">
        <Tree
          data={this.state.dataGraph}
          nodeSize={{ x: 160, y: 60 }}
          separation={{ siblings: 1, nonSiblings: 1 }}
          pathFunc="diagonal"
          translate={{ x: 20, y: 40 }}
          collapsible={false}
          zoom={0.7}
          transitionDuration={0}
          nodeSvgShape={{
            shape: "circle",
            shapeProps: {
              r: 10,
              cx: 0,
              cy: 0,
              fill: "white"
            }
          }}
          styles={{
            nodes: {
              node: {
                name: { fontSize: "12px" },
                attributes: { fontSize: "10px" }
              },
              leafNode: {
                name: { fontSize: "12px" },
                attributes: { fontSize: "10px" }
              }
            }
          }}
          onClick={(node: any) => {
            console.debug("SynthesisGraph: onNIdChange(", node.nid, ")");
            this.onNIdChange(node.nid);
            this.setState({ selectedNId: node.nid });
          }}
        />
      </div>
    );
  }
}
