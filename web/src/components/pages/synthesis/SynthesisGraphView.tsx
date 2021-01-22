import { AxiosResponse, AxiosError } from "axios";
import * as React from "react";
import Tree from "react-d3-tree";

import { SynthesisTree, api } from "services/HaskellApiService";
import { AppContext, IAppContext, SID, reLastSID } from "components/app/AppContext";

interface Paths {
  // SID -> Tree
  [key: string]: Tree;
}

interface Tree {
  name?: string;
  sid?: SID;
  attributes?: GraphAttributes;
  status?: boolean;
  children?: Tree[];
  nodeSvgShape?: any;
  nodeSvgShapeOriginal?: any;
}

interface GraphAttributes {
  [key: string]: any;
}

export const SynthesisGraphView: React.FC = () => {
  const appContext = React.useContext(AppContext) as IAppContext;

  const [dataGraph, setDataGraph] = React.useState<Tree[]>([] as Tree[]);
  const [paths, setPaths] = React.useState<Paths>({});
  const [currentSelectedNodeId, setCurrentSelectedNodeId] = React.useState<SID>("");

  const markNode = React.useCallback(
    (sid: SID, nidArray?: Paths, color?: string) => {
      if (color === undefined) color = "blue";
      if (nidArray === undefined) nidArray = paths;
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
    [paths]
  );

  const unmarkNode = React.useCallback(
    (sid: SID) => {
      if (sid === null) return;
      let tmp: string = paths[sid].nodeSvgShapeOriginal;
      let nids = paths;
      nids[sid].nodeSvgShape = tmp;
      setPaths(nids);
    },
    [paths]
  );

  const reloadSynthesisGraph = React.useCallback(() => {
    let sid = appContext.selectedSID;

    api
      .getSynthesisTree()
      .then((response: AxiosResponse<SynthesisTree>) => {
        let nidArray: Paths = {};

        let buildGraph = (gNode: Tree, dNode: SynthesisTree) => {
          let strNid: string = dNode.rootLabel.svNnid;
          gNode.name = reLastSID.exec(strNid)![0];
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
          dNode.subForest.forEach((e: SynthesisTree) => {
            if (e.rootLabel.svIsEdgesProcessed) {
              var tmp: Tree = {};
              if (gNode.children != null) {
                gNode.children.push(tmp);
                buildGraph(tmp, e);
              }
            } else {
              notProcessedCount++;
            }
          });
          if (notProcessedCount > 0) {
            var tmp: Tree = {};
            tmp.name = "..." + notProcessedCount;
            gNode.children.push(tmp);
          }
          return gNode;
        };

        let graph = buildGraph({}, response.data);
        nidArray["."] = graph;
        if (sid !== null) markNode(sid, nidArray);
        setDataGraph([graph]);
        setPaths(nidArray);
      })
      .catch((err: AxiosError) => console.log(err));
  }, [appContext.selectedSID, markNode]);

  React.useEffect(() => {
    if (currentSelectedNodeId === appContext.selectedSID && currentSelectedNodeId.length !== 0) return;
    if (appContext.selectedSID === "-" || currentSelectedNodeId.length === 0) {
      setCurrentSelectedNodeId(appContext.selectedSID);
      reloadSynthesisGraph();
      return;
    }
    if (!(appContext.selectedSID in paths)) {
      setCurrentSelectedNodeId(appContext.selectedSID);
      reloadSynthesisGraph();
      return;
    }

    unmarkNode(currentSelectedNodeId);
    markNode(appContext.selectedSID);
    setCurrentSelectedNodeId(appContext.selectedSID);
    setDataGraph([dataGraph[0]]);
    return;
  }, [
    appContext.selectedSID,
    appContext.setSID,
    currentSelectedNodeId,
    reloadSynthesisGraph,
    dataGraph,
    markNode,
    paths,
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
          appContext.setSID(node.sid);
        }}
      />
    </div>
  );
};
