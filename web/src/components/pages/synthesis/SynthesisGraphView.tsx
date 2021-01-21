import { AxiosResponse, AxiosError } from "axios";
import * as React from "react";
import Tree from "react-d3-tree";

import { SynthesisTree, haskellApiService } from "services/HaskellApiService";
import { AppContext, IAppContext, SID, reLastSID, sidSeparator } from "components/app/AppContext";

interface SID2Node {
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
  const [sid2nodeState, setSid2nodeState] = React.useState<SID2Node>({});
  const [selectedSID, setSelectedSID] = React.useState<SID>("");

  const markNode = React.useCallback(
    (sid: SID, nidArray?: SID2Node, color?: string) => {
      if (color === undefined) color = "blue";
      if (nidArray === undefined) nidArray = sid2nodeState;
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
    [sid2nodeState]
  );

  const unmarkNode = React.useCallback(
    (sid: SID) => {
      if (sid === null) return;
      let tmp: string = sid2nodeState[sid].nodeSvgShapeOriginal;
      let nids = sid2nodeState;
      nids[sid].nodeSvgShape = tmp;
      setSid2nodeState(nids);
    },
    [sid2nodeState]
  );

  const reloadSynthesisGraph = React.useCallback(() => {
    let sid = appContext.selectedSID;

    haskellApiService
      .getSynthesisTree()
      .then((response: AxiosResponse<SynthesisTree>) => {
        let sid2node: SID2Node = {};
        var t0 = performance.now();

        let buildGraph = (dNode: SynthesisTree) => {
          let label = dNode.rootLabel;
          let sid: string = label.sid;

          var gNode: Tree = {
            name: reLastSID.exec(sid)![0],
            sid: label.sid,
            status: label.isLeaf,
            attributes: {
              dec: label.decsionType,
              ch: label.duration + " / " + label.score,
            },
            nodeSvgShape: {
              shape: "circle",
              shapeProps: {
                r: 10,
                cx: 0,
                cy: 0,
                fill: label.isLeaf ? "lime" : label.isProcessed ? "black" : "white",
              },
            },
            children: [],
          };

          sid2node[label.sid] = gNode;

          gNode.children = [];
          var notProcessedCount = 0;
          dNode.subForest.forEach((e: SynthesisTree) => {
            if (e.rootLabel.isProcessed) {
              if (gNode.children != null) {
                gNode.children.push(buildGraph(e));
              }
            } else {
              notProcessedCount++;
            }
          });
          if (notProcessedCount > 0) {
            gNode.children.push({ name: "..." + notProcessedCount } as Tree);
          }
          return gNode;
        };

        let graph = buildGraph(response.data);
        sid2node[sidSeparator] = graph;
        if (sid !== null) markNode(sid, sid2node);
        setDataGraph([graph]);
        setSid2nodeState(sid2node);
      })
      .catch((err: AxiosError) => console.log(err));
  }, [appContext.selectedSID, markNode]);

  React.useEffect(() => {
    if (selectedSID === appContext.selectedSID && selectedSID.length !== 0) return;
    if (appContext.selectedSID === sidSeparator || selectedSID.length === 0) {
      setSelectedSID(appContext.selectedSID);
      reloadSynthesisGraph();
      return;
    }
    if (!(appContext.selectedSID in sid2nodeState)) {
      setSelectedSID(appContext.selectedSID);
      reloadSynthesisGraph();
      return;
    }

    unmarkNode(selectedSID);
    markNode(appContext.selectedSID);
    setSelectedSID(appContext.selectedSID);
    setDataGraph([dataGraph[0]]);
    return;
  }, [
    appContext.selectedSID,
    appContext.setSID,
    selectedSID,
    reloadSynthesisGraph,
    dataGraph,
    markNode,
    sid2nodeState,
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
        pathFunc="diagonal"
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
