import api from "../gen/rest_api.js";
import { SelectedNodeId } from "../components/app/AppContext.js";

// TODO: argument typing

export const haskellApiService = {
  getSynthesis: () => api.getSynthesis(),
  getNode: (nid: any) => api.getSynthesisByNId(nid),
  getEdge: (nid: any) => api.getSynthesisByNIdEdge(nid),
  getEdges: (nid: string) => api.getSynthesisByNIdEdges(nid),
  getTimelines: (nid: SelectedNodeId) => api.getSynthesisByNIdTimelines(nid),
  stateOfTheArtSynthesis: (nid: any) => api.postSynthesisByNIdStateOfTheArtSynthesisIO(nid),
  simpleSynthesis: (nid: any) => api.postSynthesisByNIdSimpleSynthesis(nid),
  simpleSynthesisGraph: (nid: any) => api.getSynthesisByNIdModelAlg(nid),
  smartBindSynthesisIO: (nid: any) => api.postSynthesisByNIdSmartBindSynthesisIO(nid),
  allBestThread: (nid: any, n: any) => api.postSynthesisByNIdAllBestThread(nid, n),
  obviousBindThread: (nid: any) => api.postSynthesisByNIdObviousBindThread(nid),
  runTestBench: (nid: any, name: any) => api.getSynthesisByNIdTestBenchOutput(nid, name),
  getDebugOptions: (nid: any) => api.getSynthesisByNIdDebug(nid),
  allBindsAndRefsIO: (nid: string) => api.postSynthesisByNIdAllBindsAndRefsIO(nid),
  getHistory: (nid: SelectedNodeId) => api.getSynthesisByNIdHistory(nid)
};
