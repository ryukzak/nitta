import api from "../gen/rest_api.js";

export const haskellAPI = {
  getSynthesis: (nid?: any) => {
    console.debug("hapi.getSynthesis(", nid, ")");
    return api.getSynthesis();
  },
  getNode: (nid: any) => api.getSynthesisByNId(nid),
  getEdge: (nid: any) => api.getSynthesisByNIdEdge(nid),
  getEdges: (nid: string) => api.getSynthesisByNIdEdges(nid),
  getTimelines: (nid: string) => api.getSynthesisByNIdTimelines(nid),
  simpleSynthesis: (nid: any) => api.postSynthesisByNIdSimpleSynthesis(nid),
  simpleSynthesisGraph: (nid: any) => api.getSynthesisByNIdModelAlg(nid),
  smartBindSynthesisIO: (nid: any) => api.postSynthesisByNIdSmartBindSynthesisIO(nid),
  allBestThread: (nid: any, n: any) => api.postSynthesisByNIdAllBestThread(nid, n),
  obviousBindThread: (nid: any) => api.postSynthesisByNIdObviousBindThread(nid),
  runTestBench: (nid: any, name: any) => api.getSynthesisByNIdTestBenchOutput(nid, name),
  getDebugOptions: (nid) => api.getSynthesisByNIdDebug(nid),
  allBindsAndRefsIO: (nid: string) => api.postSynthesisByNIdAllBindsAndRefsIO(nid),
  getHistory: (nid: string) => api.getSynthesisByNIdHistory(nid)
};
