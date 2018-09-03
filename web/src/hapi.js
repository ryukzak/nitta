import api from './gen/nitta-api.js'

export const hapi = {
  getSynthesis: (nid) => {
    console.debug('hapi.getSynthesis(', nid, ')')
    if (nid === undefined) {
      return api.getSynthesis()
    } else {
      return api.getSynthesisByNid(nid)
    }
  },
  getModel: (nid) => api.getSynthesisByNidModel(nid),
  simpleCompiler: (nid, onlyOneStep) => api.postSynthesisByNidSimple(nid, onlyOneStep),
  simpleCompilerOptions: (nid) => api.getSynthesisByNidSimpleOptions(nid),
  manualDecision: (nid, m) => api.postSynthesisByNidSimpleManual(nid, m),
  obviousBind: (nid) => api.postSynthesisByNidSimpleObviousBind(nid),
  simpleAllThreads: (nid, deep) => api.postSynthesisByNidSimpleAllThreads(nid, deep),
  runTestBench: (nid, name) => api.getSynthesisByNidTestBenchOutputByName(nid, name)
}
