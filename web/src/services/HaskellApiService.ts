import api from "../gen/rest_api.js";
import { SelectedNodeId } from "../components/app/AppContext.js";
import { UnitEndpointView, GraphStructure, GraphEdge, NodeView } from "../gen/types";
import { AxiosPromise } from "axios";

// TODO: argument typing
export type UnitEndpoints = UnitEndpointView<string, string>[];
export type IntermediateGraph = GraphStructure<GraphEdge>;
export type SynthesisNode = NodeView<string, string, string, string>;

export const haskellApiService = {
  getSynthesis: () => api.getSynthesis(),
  getNode: (nid: any) => api.getSynthesisByNId(nid),
  getEdge: (nid: any) => api.getSynthesisByNIdEdge(nid),
  getEdges: (nid: string) => api.getSynthesisByNIdEdges(nid),
  getTimelines: (nid: SelectedNodeId) => api.getSynthesisByNIdTimelines(nid),
  stateOfTheArtSynthesis: (nid: any) => api.postSynthesisByNIdStateOfTheArtSynthesisIO(nid),
  simpleSynthesis: (nid: any) => api.postSynthesisByNIdSimpleSynthesis(nid),
  bestStep: (nid: SelectedNodeId): AxiosPromise<SelectedNodeId> => api.postSynthesisByNIdBestStep(nid),
  getIntermediateAlg: (nid: SelectedNodeId): AxiosPromise<IntermediateGraph> => api.getSynthesisByNIdModelAlg(nid),
  smartBindSynthesisIO: (nid: any) => api.postSynthesisByNIdSmartBindSynthesisIO(nid),
  allBestThread: (nid: any, n: any) => api.postSynthesisByNIdAllBestThread(nid, n),
  obviousBindThread: (nid: any) => api.postSynthesisByNIdObviousBindThread(nid),
  runTestBench: (nid: any, name: any) => api.getSynthesisByNIdTestBenchOutput(nid, name),
  getDebugOptions: (nid: any) => api.getSynthesisByNIdDebug(nid),
  getEndpoints: (nid: SelectedNodeId): AxiosPromise<UnitEndpoints> => api.getSynthesisByNIdEndpoints(nid),
  allBindsAndRefsIO: (nid: string) => api.postSynthesisByNIdAllBindsAndRefsIO(nid),
  getHistory: (nid: SelectedNodeId) => api.getSynthesisByNIdHistory(nid),
  getPath: (nid: SelectedNodeId): AxiosPromise<SynthesisNode[]> => api.getSynthesisByNIdPath(nid)
};
