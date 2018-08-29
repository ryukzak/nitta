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
  simpleCompilerOptions: (nid) => api.getSynthesisByNidSimpleOptions(nid)
}
