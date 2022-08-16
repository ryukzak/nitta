import { AxiosResponse, AxiosError } from "axios";
import React, { FC, useContext, useState, useEffect } from "react";
import Tree from "react-d3-tree";
import { SynthesisTree, api, Sid, reLastSid, sidSeparator } from "services/HaskellApiService";
import { AppContext, IAppContext } from "app/AppContext";

type Node = {
  name: string;
  sid: Sid;
  isTerminal: boolean;
  isProcessed: boolean;
  decsionType: string;
  children: Node[];
};

function synthesisTree2D3Tree(node: SynthesisTree, knownSid: Set<Sid>, selectedSid: Sid | null): Node {
  let label = node.rootLabel;
  knownSid.add(label.sid);

  var childrens: Node[] = [];
  var skipped: number = 0;
  node.subForest.forEach((e: SynthesisTree) => {
    if (e.rootLabel.isProcessed || e.rootLabel.sid === selectedSid) {
      childrens.push(synthesisTree2D3Tree(e, knownSid, selectedSid));
    } else {
      skipped++;
    }
  });
  if (skipped > 0) {
    childrens.push({ name: `...and ${skipped} more` } as Node);
  }

  return {
    sid: label.sid,
    name: reLastSid.exec(label.sid)![0] + " " + label.decsionType,
    isProcessed: label.isProcessed,
    isTerminal: label.isTerminal,
    decsionType: label.decsionType,
    children: childrens,
  };
}

export const SynthesisGraphRender: FC = () => {
  const appContext = useContext(AppContext) as IAppContext;

  const [synthesisTree, setSynthesisTree] = useState<Node | null>(null);
  const [knownSid] = useState<Set<Sid>>(new Set());
  const [selectedSid, setSelectedSid] = useState<Sid | null>(null);
  const [dataGraph, setDataGraph] = useState<Node[]>([] as Node[]);

  useEffect(() => {
    const sid = appContext.selectedSid;
    if (knownSid.has(sid)) return;
    console.log(`SynthesisGraphRender.reloadSynthesisGraph(${sid})`);
    api
      .getSynthesisTree()
      .then((response: AxiosResponse<SynthesisTree>) => {
        let root = synthesisTree2D3Tree(response.data, knownSid, sid);
        setSynthesisTree(root);
        knownSid.add(sidSeparator);
        console.log(`SynthesisGraphRender.reloadSynthesisGraph(${sid}):done`);
      })
      .catch((err: AxiosError) => console.log(err));
  }, [appContext.selectedSid, knownSid]);

  useEffect(() => {
    const sid = appContext.selectedSid;
    if (synthesisTree && sid !== selectedSid && knownSid.has(sid)) {
      console.log(`SynthesisGraphRender.setSelectedSid(${sid}) old: ${selectedSid}`);
      setSelectedSid(sid);
      setDataGraph([synthesisTree]);
    }
  }, [appContext.selectedSid, selectedSid, knownSid, synthesisTree]);

  const renderNode = (props: any) => {
    var datum = props.nodeDatum as Node;
    var color = "white";
    if (datum.isProcessed) {
      color = "black";
    }
    if (datum.isTerminal) {
      color = "lime";
    }
    if (datum.sid === selectedSid) {
      color = "cyan";
    }

    return (
      <g>
        {datum.sid ? <circle r="10" fill={color} onClick={() => appContext.setSid(datum.sid)} /> : ""}
        <text fill="black" strokeWidth="1" x="20">
          {datum.name}
        </text>
      </g>
    );
  };

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
        renderCustomNodeElement={renderNode}
      />
    </div>
  );
};
