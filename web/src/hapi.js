import api from './gen/nitta-api.js'

export const hapi = {
  getSynthesis: (sRoot) => {
    if (sRoot === undefined) {
      return api.getSynthesis()
    } else {
      return api.getSynthesisByRsidByRsix(sRoot.sid, sRoot.six)
    }
  },
  getStep: (sRoot, six) => {
    if (six === undefined) {
      api.getSynthesisByRsidByRsixSSteps(sRoot.sid, sRoot.six)
    } else {
      api.getSynthesisByRsidByRsixSStepsBySix(sRoot.sid, sRoot.six, six)
    }
  },
  getStep2: (sRoot, six) => api.getSynthesisByRsidByRsixSStepsBySix(sRoot.sid, sRoot.six, six),
  forkSynthesis: (parent, childSix) => api.postSynthesisByRsidByRsix(parent.sid, parent.six, childSix),
  compilerStep: (sRoot, oneStep) => api.postSynthesisByRsidByRsixSSteps(sRoot.sid, sRoot.six, oneStep)

  // manualDecision: api.postSynthesisBySidSStepsBySix,
  // getStepOption: api.getSynthesisBySidSStepsBySixOptions,
}
