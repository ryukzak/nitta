import { AppContext, type IAppContext } from "app/AppContext";
import { Graphviz } from "graphviz-react";
import { type FC, useCallback, useContext } from "react";
import type { GraphEdge, GraphNode } from "services/gen/types";
import {
  api,
  type Dataflow,
  type EndpointDecision,
  type EndpointOptionData,
  type GroupBind,
  type IntermediateGraph,
  type Node,
  type SingleBind,
  type UnitEndpointsData,
} from "services/HaskellApiService";
import { DownloadTextFile } from "utils/download";
import "components/Graphviz.scss";
import { useApiRequest } from "hooks/useApiRequest";
import { useApiResponse } from "hooks/useApiResponse";
import { COMPONENT_COLORS, Color, fadeColor } from "utils/color";

/**
 * Component to display algorithm graph.
 */

export type IIntermediateViewProps = {
  functionToUnitMapping?: Map<string, string>;
  unitColors?: Map<string, string>;
  enabledFunctions?: Set<string>;
  highlightedFunctions?: Set<string>;
  highlightedDataFlows?: Set<string>;
};

interface ProcessState {
  bindeFuns: string[];
  transferedVars: string[];
}

interface Endpoints {
  sources: string[];
  targets: string[];
}

export const IntermediateView: FC<IIntermediateViewProps> = (props) => {
  const { selectedSid } = useContext(AppContext) as IAppContext;

  const algorithmGraph = useAlgorithmGraph(selectedSid);
  const procState = useProcState(selectedSid);
  const endpoints = useEndpoints(selectedSid);

  // TODO: is renderGraphJsonToDot expensive? may be a good idea to wrap expression in useMemo, otherwise it's called on
  // each rerender
  const dot = algorithmGraph
    ? renderGraphJsonToDot(
        algorithmGraph,
        procState,
        endpoints,
        props.functionToUnitMapping,
        props.unitColors,
        props.enabledFunctions,
        props.highlightedFunctions,
        props.highlightedDataFlows,
      )
    : undefined;
  return (
    <div className="bg-light border graphvizContainer">
      {dot && (
        <>
          <Graphviz
            dot={dot}
            options={{ height: 399, width: "100%", zoom: true }}
          />
          <DownloadTextFile name={"algorithm.dot"} text={dot} />
        </>
      )}
    </div>
  );
};

interface DotOptions {
  [key: string]: any;
}

function isString(obj: any) {
  return typeof obj === "string" || obj instanceof String;
}

function renderDotOptions(options: DotOptions) {
  const result = [];
  let key: string;
  for (key in options) {
    const representation: string = isString(options[key])
      ? `"${options[key]}"`
      : options[key];
    result.push(`${key}=${representation}`);
  }
  return `[${result.join("; ")}]`;
}

function isFunctionBound(bound: string[], node: GraphNode): boolean {
  if (bound.indexOf(node.function) >= 0) {
    return true;
  }
  for (const e of node.history) {
    if (bound.indexOf(e) >= 0) return true;
  }
  return false;
}

function renderGraphJsonToDot(
  json: IntermediateGraph,
  state: ProcessState,
  endpoints: Endpoints,
  functionToUnitMapping?: Map<string, string>,
  unitColors?: Map<string, string>,
  enabledFunctions?: Set<string>,
  highlightedFunctions?: Set<string>,
  highlightedDataFlows?: Set<string>,
): string {
  const lines = [
    // "rankdir=LR"
  ];
  const nodes: string[] = json.nodes.map((node) => {
    const dotOptions: any = {
      label: node.label,
      style: isFunctionBound(state.bindeFuns, node) ? "line" : "dashed",
    };
    if (functionToUnitMapping) {
      let matchedUnitFunctionName: string | null = null;
      functionToUnitMapping.keys().forEach((functionName) => {
        if (
          functionName
            .replaceAll(" ", "")
            .includes(node.function.replaceAll(" ", ""))
        )
          matchedUnitFunctionName = functionName;
      });

      if (unitColors && matchedUnitFunctionName) {
        const colorKey = unitColors.get(
          functionToUnitMapping.get(matchedUnitFunctionName)!,
        )!;
        let color = COMPONENT_COLORS[colorKey as keyof typeof COMPONENT_COLORS];
        let fontColor = color;
        let fadedColor = fadeColor(color, 0x15 / 255);
        if (color) {
          if (!enabledFunctions?.has(matchedUnitFunctionName)) {
            color = new Color({
              r: color.obj.r,
              g: color.obj.g,
              b: color.obj.b,
              a: 0.4,
            });
            fadedColor = new Color({
              r: fadedColor.obj.r,
              g: fadedColor.obj.g,
              b: fadedColor.obj.b,
              a: 0.4,
            });
            fontColor = fadeColor(color, 0.4);
          }
          dotOptions.style = "filled";
          dotOptions.fillcolor = fadedColor.toHexString();
          dotOptions.color = color.toHexString();
          dotOptions.fontcolor = fontColor.toHexString();
          dotOptions.tooltip = matchedUnitFunctionName;

          // Add highlighting for selected instructions/dataflows
          if (
            highlightedFunctions &&
            highlightedFunctions.has(matchedUnitFunctionName)
          ) {
            // dotOptions.color = "#0066cc";
            dotOptions.penwidth = 3;
          }
        }
      }
    }

    return node.id + " " + renderDotOptions(dotOptions);
  });
  function isTransfered(v: string): boolean {
    return state.transferedVars.indexOf(v) >= 0;
  }

  // Create a mapping from node ID to function name for edge highlighting
  const nodeIdToFunctionName = new Map<number, string>();
  json.nodes.forEach((node, idx) => {
    if (functionToUnitMapping) {
      let matchedUnitFunctionName: string | null = null;
      functionToUnitMapping.keys().forEach((functionName) => {
        if (
          functionName
            .replaceAll(" ", "")
            .includes(node.function.replaceAll(" ", ""))
        ) {
          matchedUnitFunctionName = functionName;
        }
      });
      if (matchedUnitFunctionName) {
        nodeIdToFunctionName.set(idx + 1, matchedUnitFunctionName);
      }
    }
  });

  const edges = json.edges.map((edge) => {
    const edgeOptions: any = {
      label: edge.label,
      style: isTransfered(edge.label) ? "line" : "dashed",
      dir: "both",
      arrowhead:
        endpoints.targets.indexOf(edge.label) >= 0 || isTransfered(edge.label)
          ? ""
          : "o",
      arrowtail:
        endpoints.sources.indexOf(edge.label) >= 0 || isTransfered(edge.label)
          ? "dot"
          : "odot",
    };

    // Add highlighting only for specific data flows (variables) related to the selected instruction
    if (highlightedDataFlows && highlightedDataFlows.has(edge.label)) {
      // edgeOptions.color = "#0066cc";
      edgeOptions.penwidth = 3;
    }

    return `${edge.from} -> ${edge.to} ` + renderDotOptions(edgeOptions);
  });

  lines.push(...nodes);
  lines.push(...edges);

  const wrap = (content: string) => `\t${content};`;
  const result = `digraph {\n${lines.map(wrap).join("\n")}\n}`;
  return result;
}

function useAlgorithmGraph(selectedSid: string): IntermediateGraph | null {
  const response = useApiRequest({
    requester: useCallback(
      () => api.getIntermediateView(selectedSid),
      [selectedSid],
    ),
  });
  const result = useApiResponse(response, makeGraphData, null);
  return result;
}

function makeGraphData(graphData: IntermediateGraph): IntermediateGraph | null {
  return {
    nodes: graphData.nodes.map((nodeData: GraphNode, index: number) => {
      return {
        id: index + 1,
        label: String(nodeData.label),
        function: nodeData.function,
        history: nodeData.history,
        nodeColor: "",
        nodeShape: "",
        fontSize: "",
        nodeSize: "",
      };
    }),
    edges: graphData.edges.map((edgeData: GraphEdge) => {
      return edgeData;
    }),
  };
}

function useProcState(selectedSid: string): ProcessState {
  const response = useApiRequest({
    requester: useCallback(() => api.getRootPath(selectedSid), [selectedSid]),
  });
  const result = useApiResponse(response, makeProcState, defaultProcState);
  return result;
}

const defaultProcState: ProcessState = { bindeFuns: [], transferedVars: [] };

function makeProcState(nodes: Node[]): ProcessState {
  const procState: ProcessState = { bindeFuns: [], transferedVars: [] };
  nodes.forEach((n: Node) => {
    if (n.decision.tag === "DataflowDecisionView") {
      const targets = (n.decision as Dataflow).targets;
      targets.forEach((target: [string, EndpointDecision]) => {
        procState.transferedVars.push(target[1].epRole.contents as string);
      });
    }
    if (n.decision.tag === "SingleBindView") {
      const d = n.decision as SingleBind;
      procState.bindeFuns.push(d.function.fvFun, ...d.function.fvHistory);
    }

    if (n.decision.tag === "GroupBindView") {
      const groupBind = n.decision as GroupBind;
      Object.values(groupBind.bindGroup).forEach((functions) => {
        functions?.forEach((func) => {
          procState.bindeFuns.push(func.fvFun, ...func.fvHistory);
        });
      });
    }
  });
  return procState;
}

function useEndpoints(selectedSid: string): Endpoints {
  const response = useApiRequest({
    requester: useCallback(() => api.getEndpoints(selectedSid), [selectedSid]),
  });
  const result = useApiResponse(response, collectEndpoints, defaultEndpoints);
  return result;
}

const defaultEndpoints: Endpoints = { sources: [], targets: [] };

function collectEndpoints(data: UnitEndpointsData[]): Endpoints {
  const endpoints: Endpoints = { sources: [], targets: [] };
  data.forEach((eps: UnitEndpointsData) => {
    eps.unitEndpoints.forEach((e: EndpointOptionData) => {
      const role = e.epRole;
      if (role.tag === "Source") {
        endpoints.sources.push(...role.contents);
      }
      if (role.tag === "Target") {
        endpoints.targets.push(role.contents);
      }
    });
  });
  return endpoints;
}
