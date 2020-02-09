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
  TestbenchReport
} from "../gen/types";
import { AxiosPromise } from "axios";

// TODO: argument typing
export type UnitEndpoints = UnitEndpointView<string, string>[];
export type IntermediateGraph = GraphStructure<GraphEdge>;
export type SynthesisNode = NodeView<string, string, string, string>;
export type Edge = EdgeView<string, string, number, number>;
export type TestBenchReport = TestbenchReport<string, number>;

export const haskellApiService = {
  // SynthesisAPI
  getSynthesisTree: (): AxiosPromise<TreeView<SynthesisNodeView>> => api.getSynthesisTree(),

  // GetSynthesis
  getNode: (nid: NodeId): AxiosPromise<any> => api.getSynthesisByNId(nid),
  getPath: (nid: NodeId): AxiosPromise<SynthesisNode[]> => api.getSynthesisByNIdPath(nid),
  getOriginEdge: (nid: NodeId): AxiosPromise<any> => api.getSynthesisByNIdOriginEdge(nid),
  getEdges: (nid: NodeId): AxiosPromise<Edge[]> => api.getSynthesisByNIdEdges(nid),

  getIntermediateView: (nid: NodeId): AxiosPromise<IntermediateGraph> => api.getSynthesisByNIdIntermediateView(nid),
  getTimelines: (nid: NodeId): AxiosPromise<any> => api.getSynthesisByNIdTimelines(nid),

  getDebugOptions: (nid: NodeId): AxiosPromise<any> => api.getSynthesisByNIdDebug(nid),
  getPUEndpoints: (nid: NodeId): AxiosPromise<UnitEndpoints> => api.getSynthesisByNIdPuEndpoints(nid),

  // PostSynthesis
  stateOfTheArtSynthesis: (nid: NodeId): AxiosPromise<NodeId> => api.postSynthesisByNIdStateOfTheArtSynthesisIO(nid),
  simpleSynthesis: (nid: NodeId): AxiosPromise<NodeId> => api.postSynthesisByNIdSimpleSynthesis(nid),
  bestStep: (nid: NodeId): AxiosPromise<NodeId> => api.postSynthesisByNIdBestStep(nid),
  allBestThread: (nid: NodeId, n: number): AxiosPromise<NodeId> => api.postSynthesisByNIdAllBestThread(nid, n),
  obviousBindThread: (nid: NodeId): AxiosPromise<NodeId> => api.postSynthesisByNIdObviousBindThread(nid),
  allBindsAndRefsIO: (nid: NodeId): AxiosPromise<NodeId> => api.postSynthesisByNIdAllBindsAndRefsIO(nid),
  smartBindSynthesisIO: (nid: NodeId): AxiosPromise<NodeId> => api.postSynthesisByNIdSmartBindSynthesisIO(nid),

  // testbench
  runTestBench: (nid: NodeId, name: string): AxiosPromise<TestBenchReport | null> => api.postTestbenchByNId(nid, name)
};
