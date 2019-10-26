import * as React from "react";
import Tree from "react-d3-tree";
import { haskellApiService } from "../../../services/HaskellApiService";
import { AppContext, IAppContext } from "../../app/AppContext";
import { NId, SynthesisNodeView } from "../../../gen/types";

interface JsonResponse {
  [key: string]: any;
}

interface JsonObjId {
  [key: string]: Graph;
}

interface Graph {
  name?: string;
  nid?: NId;
  attributes?: JsonResponse;
  status?: boolean;
  children?: Graph[];
  nodeSvgShape?: any;
  nodeSvgShapeOriginal?: any;
}

export const SynthesisGraphView: React.FC = () => {
  const appContext = React.useContext(AppContext) as IAppContext;

  const [dataGraph, setDataGraph] = React.useState<Graph[]>([] as Graph[]);
  const [nIds, setNIds] = React.useState<JsonObjId>({});
  const [currentSelectedNId, setCurrentSelectedNId] = React.useState<string | null>(null);

  const markNode = React.useCallback(
    (nid: any, nidArray?: any, color?: any) => {
      if (color === undefined) color = "blue";
      if (nidArray === undefined) nidArray = nIds;
      if (nid === null || nidArray === null) return;

      if (color === "blue") {
        nidArray[nid].nodeSvgShapeOriginal = nidArray[nid].nodeSvgShape;
      }
      nidArray[nid].nodeSvgShape = {
        shape: "circle",
        shapeProps: {
          r: 10,
          cx: 0,
          cy: 0,
          fill: color
        }
      };
    },
    [nIds]
  );

  const unmarkNode = React.useCallback(
    (nid: any) => {
      if (nid === null) return;
      let tmp: string = nIds[nid].nodeSvgShapeOriginal;
      let nids = nIds;
      nids[nid].nodeSvgShape = tmp;
      setNIds(nids);
    },
    [nIds]
  );

  const reloadSynthesisGraph = React.useCallback(() => {

    let reLastNidStep = /-[^-]*$/; // nInSeparator
    let nid = appContext.selectedNodeId;

    // FIXME: Need to replace type of the SynthesisNodeView.svNnid (dNode[0].svNnid) from number[] to string. Now svNnid declared as number[], but request are responsing string.
    haskellApiService
      .getSynthesis()
      .then((response: any) => {
        let nidArray: JsonObjId = {};
        let buildGraph = (gNode: Graph, dNode: [SynthesisNodeView, Array<SynthesisNodeView>]) => {
          // @ts-ignore
          gNode.name = reLastNidStep.exec(dNode[0].svNnid)![0];
          gNode.nid = dNode[0].svNnid;
          // @ts-ignore
          nidArray[dNode[0].svNnid] = gNode;
          if (dNode[0].svIsEdgesProcessed) markNode(gNode.nid, nidArray, "black");
          if (dNode[0].svIsComplete) markNode(gNode.nid, nidArray, "lime");
          gNode.attributes = {
            dec: dNode[0].svOptionType,
            ch: dNode[0].svDuration + " / " + dNode[0].svCharacteristic
          };
          gNode.status = dNode[0].svIsComplete;
          dNode[0].svCntx.forEach((e: string, i: number) => {
            gNode.attributes![i] = e;
          });
          gNode.children = [];
          dNode[1].forEach((e: any) => {
            var tmp: Graph = {};
            if (gNode.children != null) {
              gNode.children.push(tmp);
              buildGraph(tmp, e);
            }
          });
          return gNode;
        };

        let graph = buildGraph({}, response.data);
        nidArray["."] = graph;
        if (nid !== null) markNode(nid, nidArray);
        setDataGraph([graph]);
        setNIds(nidArray);
      })
      .catch((err: any) => console.log(err));
  }, [appContext.selectedNodeId, markNode]);

  React.useEffect(() => {
    if (currentSelectedNId === appContext.selectedNodeId) return;

    if (appContext.selectedNodeId === "-" || currentSelectedNId === null) {
      setCurrentSelectedNId( appContext.selectedNodeId );
      reloadSynthesisGraph();
      return;
    }
    if (!(appContext.selectedNodeId in nIds)) {
      setCurrentSelectedNId(appContext.selectedNodeId);
      reloadSynthesisGraph();
      return;
    }
    else {
      unmarkNode(currentSelectedNId);
      markNode(appContext.selectedNodeId);

      setCurrentSelectedNId(appContext.selectedNodeId);
      setDataGraph([dataGraph[0]]);
      return;
    }
  }, [
    appContext.selectedNodeId,
    appContext.selectNode,
    currentSelectedNId,
    reloadSynthesisGraph,
    dataGraph,
    markNode,
    nIds,
    unmarkNode
  ]);

  if (!dataGraph === null || dataGraph.length === 0) {
    return (
      <div className="h-100 d-flex align-items-center justify-content-center text-black-50">
        <h1>Empty graph</h1>
      </div>
    );
  } else {
    return (
      <div className="h-100">
        <Tree
          data={dataGraph}
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
            appContext.selectNode(node.nid);
          }}
        />
      </div>
    );
  }
};
