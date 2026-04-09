import { AppContext, type IAppContext } from "app/AppContext";
import type { AxiosError, AxiosResponse } from "axios";
import {
  type FC,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useState,
} from "react";
import Tree from "react-d3-tree";
import {
  api,
  reLastSid,
  type Sid,
  type SynthesisTree,
  sidSeparator,
} from "services/HaskellApiService";

type Node = {
  name: string;
  sid: Sid;
  isTerminal: boolean;
  isProcessed: boolean;
  decsionType: string;
  children: Node[];
  isHidden?: boolean;
  allChildrenCount?: number;
  hiddenChildrenCount?: number;
};

export const SynthesisGraphRender: FC = () => {
  const appContext = useContext(AppContext) as IAppContext;
  const [synthesisTree, setSynthesisTree] = useState<Node | null>(null);
  const [knownSid] = useState<Set<Sid>>(new Set());
  const [selectedSid, setSelectedSid] = useState<Sid | null>(null);
  const [dataGraph, setDataGraph] = useState<Node[]>([] as Node[]);
  const [hiddenNodes, setHiddenNodes] = useState<Set<Sid>>(new Set());

  const synthesisTree2D3Tree = useCallback(
    (node: SynthesisTree, knownSid: Set<Sid>, selectedSid: Sid | null) => {
      const label = node.rootLabel;
      knownSid.add(label.sid);

      const children: Node[] = [];
      let hiddenChildrenCount: number = 0;
      node.subForest.forEach((e: SynthesisTree) => {
        children.push(synthesisTree2D3Tree(e, knownSid, selectedSid));
        if (!(e.rootLabel.isProcessed || e.rootLabel.sid === selectedSid)) {
          hiddenChildrenCount++;
          setHiddenNodes((prev) => {
            const newSet = new Set(prev);
            newSet.add(e.rootLabel.sid);
            return newSet;
          });
        }
      });

      return {
        sid: label.sid,
        name: reLastSid.exec(label.sid)![0] + " " + label.decsionType,
        isProcessed: label.isProcessed,
        isTerminal: label.isTerminal,
        decsionType: label.decsionType,
        children: children,
        isHidden: hiddenNodes.has(label.sid),
        allChildrenCount: children.length,
        hiddenChildrenCount: hiddenChildrenCount,
      };
    },
    [hiddenNodes.has],
  );

  useEffect(() => {
    const sid = appContext.selectedSid;
    if (knownSid.has(sid)) return;
    console.log(`SynthesisGraphRender.reloadSynthesisGraph(${sid})`);
    api
      .getSynthesisTree()
      .then((response: AxiosResponse<SynthesisTree>) => {
        const root = synthesisTree2D3Tree(response.data, knownSid, sid);
        setSynthesisTree(root);
        knownSid.add(sidSeparator);
        console.log(`SynthesisGraphRender.reloadSynthesisGraph(${sid}):done`);
      })
      .catch((err: AxiosError) => console.log(err));
  }, [appContext.selectedSid, knownSid, synthesisTree2D3Tree]);

  useEffect(() => {
    const sid = appContext.selectedSid;
    if (synthesisTree && sid !== selectedSid && knownSid.has(sid)) {
      console.log(
        `SynthesisGraphRender.setSelectedSid(${sid}) old: ${selectedSid}`,
      );
      setSelectedSid(sid);
      setDataGraph([synthesisTree]);
    }
  }, [appContext.selectedSid, selectedSid, knownSid, synthesisTree]);

  const findNode = (nodes: Node[], id: Sid): Node | null => {
    for (const node of nodes) {
      if (node.sid === id) return node;
      if (node.children) {
        const found = findNode(node.children, id);
        if (found) return found;
      }
    }
    return null;
  };

  const handleNodeButtonClick = (node: Node, event: React.MouseEvent) => {
    event.stopPropagation();

    const originalNode = findNode(dataGraph, node.sid);
    if (originalNode === null) return;

    if (node.allChildrenCount === node.hiddenChildrenCount) {
      setHiddenNodes((prev) => {
        const newSet = new Set(prev);
        originalNode.children.forEach((child) => {
          newSet.delete(child.sid);
        });
        return newSet;
      });
    } else {
      setHiddenNodes((prev) => {
        const newSet = new Set(prev);
        originalNode.children.forEach((child) => {
          newSet.add(child.sid);
        });
        return newSet;
      });
    }
  };

  const handleNodeTitleClick = (nodeId: Sid, event: React.MouseEvent) => {
    event.stopPropagation();
    if (nodeId === "-") return;
    setHiddenNodes((prev) => {
      const newSet = new Set(prev);
      if (newSet.has(nodeId)) {
        newSet.delete(nodeId);
      } else {
        newSet.add(nodeId);
      }
      return newSet;
    });
  };

  const processHiddenChildren = useCallback(
    (node: Node, hidden: Set<Sid>): Node => {
      const visibleChildren: Node[] = [];
      let hiddenChildrenCount = 0;
      node.children.forEach((c) => {
        if (hidden.has(c.sid)) {
          hiddenChildrenCount++;
        } else visibleChildren.push(processHiddenChildren(c, hidden));
      });

      if (
        hiddenChildrenCount > 0 &&
        hiddenChildrenCount < node.children.length
      ) {
        const moreNode: Node = {
          name: `...and ${hiddenChildrenCount} more`,
          sid: null as any,
          isTerminal: false,
          isProcessed: false,
          decsionType: "",
          children: [],
          isHidden: false,
        };
        visibleChildren.push(moreNode);
      }
      return {
        ...node,
        isHidden: hidden.has(node.sid),
        children: visibleChildren,
        allChildrenCount: node.children.length,
        hiddenChildrenCount: hiddenChildrenCount,
      };
    },
    [],
  );

  const displayGraph = useMemo(() => {
    if (dataGraph.length === 0) return dataGraph;
    const mapped = dataGraph.map((node) =>
      processHiddenChildren(node, hiddenNodes),
    );
    return mapped.filter((n) => !n.isHidden);
  }, [dataGraph, hiddenNodes, processHiddenChildren]);

  const renderNode = (props: any) => {
    const datum = props.nodeDatum as Node;
    if (datum.isHidden) return;
    let color = "white";
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
        {datum.sid ? (
          <>
            {datum.allChildrenCount && datum.allChildrenCount > 0 && (
              <>
                <text
                  x="-25"
                  y="0"
                  fill="black"
                  fontSize="16"
                  fontWeight="bold"
                  strokeWidth="0"
                  cursor="pointer"
                  onClick={(e) => handleNodeButtonClick(datum, e as any)}
                  role="button"
                  tabIndex={0}
                >
                  {datum.hiddenChildrenCount === datum.allChildrenCount
                    ? "+"
                    : "-"}
                </text>
              </>
            )}
            <circle
              r="10"
              fill={color}
              onClick={() => appContext.setSid(datum.sid)}
              role={"treeitem"}
            />
          </>
        ) : (
          ""
        )}
        <text
          fill="black"
          strokeWidth="1"
          x="20"
          cursor="pointer"
          onClick={(e) => handleNodeTitleClick(datum.sid, e as any)}
          role="button"
          tabIndex={0}
        >
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
        data={displayGraph}
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
