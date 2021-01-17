import { AxiosPromise } from "axios";

import api from "gen/rest_api.js";
import { SID } from "components/app/AppContext.js";

import { TreeView, SynthesisNodeView } from "../gen/types";
import { NodeView, DecisionView, IRootView, IBindDecisionView, IDataflowDecisionView } from "../gen/types";
import { EndpointStView, GraphStructure, GraphEdge, TestbenchReportView } from "../gen/types";

export type SynthesisTree = TreeView<SynthesisNodeView>;
export type Node = NodeView<string, string, number, number>;
export type Decision = DecisionView;
export type Root = IRootView;
export type Bind = IBindDecisionView;
export type Dataflow = IDataflowDecisionView;

export type EndpointSts = EndpointStView<string, string>[];
export type IntermediateGraph = GraphStructure<GraphEdge>;
export type TestBenchReport = TestbenchReportView<string, number>;

export const haskellApiService = {
  getSynthesisTree: (): AxiosPromise<TreeView<SynthesisNodeView>> => api.getSynthesisTree(),

  // Synthesis tree navigation
  getRootPath: (sid: SID): AxiosPromise<Node[]> => api.getNodeBySidRootPath(sid),
  getParentEdge: (sid: SID): AxiosPromise<Node> => api.getNodeBySidParentEdge(sid),
  getSubforest: (sid: SID): AxiosPromise<Node[]> => api.getNodeBySidSubForest(sid),

  // Synthesis node inspections
  getNode: (sid: SID): AxiosPromise<Node> => api.getNodeBySid(sid),
  getIntermediateView: (sid: SID): AxiosPromise<IntermediateGraph> => api.getNodeBySidIntermediateView(sid),
  getTimelines: (sid: SID): AxiosPromise<any> => api.getNodeBySidProcessTimelines(sid),
  getEndpoints: (sid: SID): AxiosPromise<EndpointSts> => api.getNodeBySidEndpoints(sid),
  getDebugInfo: (sid: SID): AxiosPromise<any> => api.getNodeBySidDebug(sid),
  runTestBench: (sid: SID, name: string, loopsNumber: number): AxiosPromise<TestBenchReport | null> =>
    api.postNodeBySidTestbench(sid, name, loopsNumber),

  // Synthesis methods
  stateOfTheArtSynthesis: (sid: SID): AxiosPromise<SID> => api.postNodeBySidStateOfTheArtSynthesisIO(sid),
  simpleSynthesis: (sid: SID): AxiosPromise<SID> => api.postNodeBySidSimpleSynthesis(sid),
  smartBindSynthesisIO: (sid: SID): AxiosPromise<SID> => api.postNodeBySidSmartBindSynthesisIO(sid),

  // Synthesis practice
  bestStep: (sid: SID): AxiosPromise<SID> => api.postNodeBySidBestStep(sid),
  allBestThread: (sid: SID, n: number): AxiosPromise<SID> => api.postNodeBySidAllBestThreads(sid, n),
  obviousBindThread: (sid: SID): AxiosPromise<SID> => api.postNodeBySidObviousBindThread(sid),
  allBindsAndRefsIO: (sid: SID): AxiosPromise<SID> => api.postNodeBySidAllBindsAndRefsIO(sid),
};
