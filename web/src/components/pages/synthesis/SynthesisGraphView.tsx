import * as React from "react";
import Tree from "react-d3-tree";
import { haskellApiService } from "../../../services/HaskellApiService";
import { AppContext, IAppContext, NodeId, reLastNidStep } from "../../app/AppContext";
import { SynthesisNodeView, TreeView, NId } from "../../../gen/types";
import { AxiosResponse, AxiosError } from "axios";

// FIXME: review, refactor (naming!)

interface Ids {
  [key: string]: Graph;
}

interface GraphAttributes {
  [key: string]: any;
}

interface Graph {
  name?: string;
  sid?: NId;
  attributes?: GraphAttributes;
  status?: boolean;
  children?: Graph[];
  nodeSvgShape?: any;
  nodeSvgShapeOriginal?: any;
}

export const SynthesisGraphView: React.FC = () => {
  const appContext = React.useContext(AppContext) as IAppContext;

  const [dataGraph, setDataGraph] = React.useState<Graph[]>([] as Graph[]);
  const [nIds, setNIds] = React.useState<Ids>({});
  const [currentSelectedNodeId, setCurrentSelectedNodeId] = React.useState<NodeId>("");

  const markNode = React.useCallback(
    (sid: NodeId, nidArray?: Ids, color?: string) => {
      if (color === undefined) color = "blue";
      if (nidArray === undefined) nidArray = nIds;
      if (nidArray === null) return;

      if (color === "blue") {
        nidArray[sid].nodeSvgShapeOriginal = nidArray[sid].nodeSvgShape;
      }
      nidArray[sid].nodeSvgShape = {
        shape: "circle",
        shapeProps: {
          r: 10,
          cx: 0,
          cy: 0,
          fill: color,
        },
      };
    },
    [nIds]
  );

  const unmarkNode = React.useCallback(
    (sid: NodeId) => {
      if (sid === null) return;
      let tmp: string = nIds[sid].nodeSvgShapeOriginal;
      let nids = nIds;
      nids[sid].nodeSvgShape = tmp;
      setNIds(nids);
    },
    [nIds]
  );

  const reloadSynthesisGraph = React.useCallback(() => {
    let sid = appContext.selectedNodeId;

    haskellApiService
      .getSynthesisTree()
      .then((response: AxiosResponse<TreeView<SynthesisNodeView>>) => {
        let nidArray: Ids = {};

        let buildGraph = (gNode: Graph, dNode: TreeView<SynthesisNodeView>) => {
          let strNid: string = dNode.rootLabel.svNnid;
          gNode.name = reLastNidStep.exec(strNid)![0];
          gNode.sid = dNode.rootLabel.svNnid;
          nidArray[strNid] = gNode;
          if (dNode.rootLabel.svIsEdgesProcessed) markNode(strNid, nidArray, "black");
          if (dNode.rootLabel.svIsComplete) markNode(strNid, nidArray, "lime");
          gNode.attributes = {
            dec: dNode.rootLabel.svOptionType,
            ch: dNode.rootLabel.svDuration + " / " + dNode.rootLabel.svCharacteristic,
          };
          gNode.status = dNode.rootLabel.svIsComplete;
          gNode.children = [];
          var notProcessedCount = 0;
          dNode.subForest.forEach((e: TreeView<SynthesisNodeView>) => {
            if (e.rootLabel.svIsEdgesProcessed) {
              var tmp: Graph = {};
              if (gNode.children != null) {
                gNode.children.push(tmp);
                buildGraph(tmp, e);
              }
            } else {
              notProcessedCount++;
            }
          });
          if (notProcessedCount > 0) {
            var tmp: Graph = {};
            tmp.name = "..." + notProcessedCount;
            gNode.children.push(tmp);
          }
          return gNode;
        };

        let graph = buildGraph({}, response.data);
        nidArray["."] = graph;
        if (sid !== null) markNode(sid, nidArray);
        setDataGraph([graph]);
        setNIds(nidArray);
      })
      .catch((err: AxiosError) => console.log(err));
  }, [appContext.selectedNodeId, markNode]);

  React.useEffect(() => {
    if (currentSelectedNodeId === appContext.selectedNodeId && currentSelectedNodeId.length !== 0) return;
    if (appContext.selectedNodeId === "-" || currentSelectedNodeId.length === 0) {
      setCurrentSelectedNodeId(appContext.selectedNodeId);
      reloadSynthesisGraph();
      return;
    }
    if (!(appContext.selectedNodeId in nIds)) {
      setCurrentSelectedNodeId(appContext.selectedNodeId);
      reloadSynthesisGraph();
      return;
    }

    unmarkNode(currentSelectedNodeId);
    markNode(appContext.selectedNodeId);
    setCurrentSelectedNodeId(appContext.selectedNodeId);
    setDataGraph([dataGraph[0]]);
    return;
  }, [
    appContext.selectedNodeId,
    appContext.selectNode,
    currentSelectedNodeId,
    reloadSynthesisGraph,
    dataGraph,
    markNode,
    nIds,
    unmarkNode,
  ]);

  if (!dataGraph === null || dataGraph.length === 0) {
    return (
      <div className="h-100 d-flex align-items-center justify-content-center text-black-50">
        <h1>Empty graph</h1>
      </div>
    );
  }
  return (
    <div className="h-100">
      <Tree
        data={dataGraph}
        nodeSize={{ x: 160, y: 60 }}
        separation={{ siblings: 1, nonSiblings: 1 }}
        pathFunc="straight"
        translate={{ x: 20, y: 40 }}
        collapsible={false}
        zoom={0.7}
        transitionDuration={0}
        nodeSvgShape={{
          shape: "rect",
          shapeProps: {
            r: 10,
            cx: 0,
            cy: 0,
            fill: "white",
          },
        }}
        styles={{
          nodes: {
            node: {
              name: { fontSize: "12px" },
              attributes: { fontSize: "10px" },
            },
            leafNode: {
              name: { fontSize: "12px" },
              attributes: { fontSize: "10px" },
            },
          },
        }}
        onClick={(node: any) => {
          appContext.selectNode(node.sid);
        }}
      />
    </div>
  );
};
