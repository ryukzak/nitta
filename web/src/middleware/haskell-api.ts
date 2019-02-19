import api from "../gen/rest_api.js";

export const haskellAPI = {
  getSynthesis: (nid?: any) => {
    console.debug("hapi.getSynthesis(", nid, ")");
    return api.getSynthesis();
  },
  getNode: (nid: any) => api.getSynthesisByNId(nid),
  getEdge: (nid: any) => api.getSynthesisByNIdEdge(nid),
  getEdges: (nid: any) => api.getSynthesisByNIdEdges(nid),
  simpleSynthesis: (nid: any) => api.postSynthesisByNIdSimpleSynthesis(nid),
  smartBindSynthesisIO: (nid: any) => api.postSynthesisByNIdSmartBindSynthesisIO(nid),
  allBestThread: (nid: any, n: any) => api.postSynthesisByNIdAllBestThread(nid, n),
  obviousBindThread: (nid: any) => api.postSynthesisByNIdObviousBindThread(nid),
  runTestBench: (nid: any, name: any) => api.getSynthesisByNIdTestBenchOutput(nid, name)
};
