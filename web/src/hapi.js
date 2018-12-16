import api from './gen/rest_api.js'

export const hapi = {
  getSynthesis: (nid) => {
    console.debug('hapi.getSynthesis(', nid, ')')
    return api.getSynthesis()
  },
  getNode: (nid) => api.getSynthesisByNId(nid),
  getEdges: (nid) => api.getSynthesisByNIdEdges(nid),
  simpleSynthesis: (nid) => api.postSynthesisByNIdSimpleSynthesis(nid),
  allBestThread: (nid, n) => api.postSynthesisByNIdAllBestThread(nid, n),
  obviousBindThread: (nid) => api.postSynthesisByNIdObviousBindThread(nid),
  runTestBench: (nid, name) => api.getSynthesisByNIdTestBenchOutput(nid, name)
}
