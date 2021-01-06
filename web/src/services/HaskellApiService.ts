import api from "../gen/rest_api.js";
import { NodeId } from "../components/app/AppContext.js";
import {
  UnitEndpointView,
  GraphStructure,
  GraphEdge,
  NodeView,
  EdgeView,
  TreeView,
  SynthesisNodeView,
  TestbenchReportView,
} from "../gen/types";
import { AxiosPromise } from "axios";

// TODO: argument typing
export type UnitEndpoints = UnitEndpointView<string, string>[];
export type IntermediateGraph = GraphStructure<GraphEdge>;
export type SynthesisNode = NodeView<string, string, string, string>;
export type Edge = EdgeView<string, string, number, number>;
export type TestBenchReport = TestbenchReportView<string, number>;

export const haskellApiService = {
  getSynthesisTree: (): AxiosPromise<TreeView<SynthesisNodeView>> => api.getSynthesisTree(),

  // Synthesis tree navigation
  getRootPath: (nid: NodeId): AxiosPromise<SynthesisNode[]> => api.getNodeByNIdRootPath(nid),
  getParentEdge: (nid: NodeId): AxiosPromise<Edge> => api.getNodeByNIdParentEdge(nid),
  getChildEdges: (nid: NodeId): AxiosPromise<Edge[]> => api.getNodeByNIdChildEdges(nid),

  // Synthesis node inspections
  getNode: (nid: NodeId): AxiosPromise<SynthesisNode> => api.getNodeByNId(nid),
  getIntermediateView: (nid: NodeId): AxiosPromise<IntermediateGraph> => api.getNodeByNIdIntermediateView(nid),
  getTimelines: (nid: NodeId): AxiosPromise<any> => api.getNodeByNIdProcessTimelines(nid),
  getEndpoints: (nid: NodeId): AxiosPromise<UnitEndpoints> => api.getNodeByNIdEndpoints(nid),
  getDebugInfo: (nid: NodeId): AxiosPromise<any> => api.getNodeByNIdDebug(nid),
  runTestBench: (nid: NodeId, name: string): AxiosPromise<TestBenchReport | null> =>
    api.postNodeByNIdTestbench(nid, name),

  // Synthesis methods
  stateOfTheArtSynthesis: (nid: NodeId): AxiosPromise<NodeId> => api.postNodeByNIdStateOfTheArtSynthesisIO(nid),
  simpleSynthesis: (nid: NodeId): AxiosPromise<NodeId> => api.postNodeByNIdSimpleSynthesis(nid),
  smartBindSynthesisIO: (nid: NodeId): AxiosPromise<NodeId> => api.postNodeByNIdSmartBindSynthesisIO(nid),

  // Synthesis practice
  bestStep: (nid: NodeId): AxiosPromise<NodeId> => api.postNodeByNIdBestStep(nid),
  allBestThread: (nid: NodeId, n: number): AxiosPromise<NodeId> => api.postNodeByNIdAllBestThreads(nid, n),
  obviousBindThread: (nid: NodeId): AxiosPromise<NodeId> => api.postNodeByNIdObviousBindThread(nid),
  allBindsAndRefsIO: (nid: NodeId): AxiosPromise<NodeId> => api.postNodeByNIdAllBindsAndRefsIO(nid),
};
