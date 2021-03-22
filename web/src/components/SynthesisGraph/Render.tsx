import { AxiosResponse, AxiosError } from "axios";
import * as React from "react";
import { Tree as D3Tree } from "react-d3-tree";

import { SynthesisTree, api, SID, reLastSID, sidSeparator } from "services/HaskellApiService";
import { AppContext, IAppContext } from "app/AppContext";

type Tree = {
  name: string;
  sid: SID;
  attributes: GraphAttributes;
  isLeaf: boolean;
  isProcessed: boolean;
  children: Tree[];
  nodeSvgShape: GraphAttributes;
  nodeSvgShapeOriginal?: GraphAttributes;
};

type GraphAttributes = {
  [key: string]: any;
};

function synthesisTree2D3Tree(node: SynthesisTree, knownSID: Set<SID>): Tree {
  let label = node.rootLabel;
  knownSID.add(label.sid);

  var childrens: Tree[] = [];
  node.subForest.forEach((e: SynthesisTree) => {
    childrens.push(synthesisTree2D3Tree(e, knownSID));
  });

  return {
    sid: label.sid,
    name: reLastSID.exec(label.sid)![0],
    isProcessed: label.isProcessed,
    isLeaf: label.isLeaf,
    attributes: {
      dec: label.decsionType,
      ch: label.duration + " / " + label.score,
    },
    nodeSvgShape: label.isLeaf ? nodeShape("lime") : label.isProcessed ? nodeShape("black") : nodeShape("white"),
    children: childrens,
  };
}

function selectCurrentNode(currentSID: SID, node: Tree): Tree {
  var childrens: Tree[] = [];

  var skipped = 0;
  node.children.forEach((e: Tree) => {
    if (e.isProcessed || e.sid === currentSID) {
      childrens.push(selectCurrentNode(currentSID, e));
    } else {
      skipped++;
    }
  });
  if (skipped > 0) {
    childrens.push({ name: `...and ${skipped} more` } as Tree);
  }

  return {
    name: node.name,
    isProcessed: node.isProcessed,
    sid: node.sid,
    isLeaf: node.isLeaf,
    attributes: node.attributes,
    nodeSvgShape: currentSID === node.sid ? nodeShape("cyan") : node.nodeSvgShape,
    children: childrens,
  };
}

export const SynthesisGraphRender: React.FC = () => {
  const appContext = React.useContext(AppContext) as IAppContext;

  const [synthesisTree, setSynthesisTree] = React.useState<Tree | null>(null);
  const [knownSID] = React.useState<Set<SID>>(new Set());

  const [currentSID, setCurrentSID] = React.useState<SID | null>(null);

  const [dataGraph, setDataGraph] = React.useState<Tree[]>([] as Tree[]);

  React.useEffect(() => {
    const sid = appContext.selectedSID;
    if (knownSID.has(sid)) return;
    console.log(`SynthesisGraphRender.reloadSynthesisGraph(${sid})`);
    api
      .getSynthesisTree()
      .then((response: AxiosResponse<SynthesisTree>) => {
        let root = synthesisTree2D3Tree(response.data, knownSID);
        setSynthesisTree(root);
        knownSID.add(sidSeparator);
        console.log(`SynthesisGraphRender.reloadSynthesisGraph(${sid}):done`);
      })
      .catch((err: AxiosError) => console.log(err));
  }, [appContext.selectedSID, knownSID]);

  React.useEffect(() => {
    const sid = appContext.selectedSID;
    if (synthesisTree && sid !== currentSID && knownSID.has(sid)) {
      console.log(`SynthesisGraphRender.setCurrentSID(${sid}) old: ${currentSID}`);
      setCurrentSID(sid);
      setDataGraph([selectCurrentNode(sid, synthesisTree)]);
    }
  }, [appContext.selectedSID, currentSID, knownSID, synthesisTree]);

  if (!dataGraph === null || dataGraph.length === 0) {
    return (
      <div className="h-100 d-flex align-items-center justify-content-center text-black-50">
        <h1>Empty graph</h1>
      </div>
    );
  }
  return (
    <div className="h-100">
      <D3Tree
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

function nodeShape(color: string): GraphAttributes {
  return {
    shape: "circle",
    shapeProps: {
      r: 10,
      cx: 0,
      cy: 0,
      fill: color,
    },
  };
}
