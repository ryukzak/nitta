import api from "../gen/rest_api.js";
import { NodeId } from "../components/app/AppContext.js";
import {
  EndpointStView,
  GraphStructure,
  GraphEdge,
  NodeView,
  IRootView,
  IBindDecisionView,
  IDataflowDecisionView,
  IRefactorDecisionView,
  TreeView,
  SynthesisNodeView,
  TestbenchReportView,
} from "../gen/types";
import { AxiosPromise } from "axios";

// TODO: argument typing
export type EndpointSts = EndpointStView<string, string>[];
export type IntermediateGraph = GraphStructure<GraphEdge>;
export type Node = NodeView<string, string, number, number>;
export type TestBenchReport = TestbenchReportView<string, number>;

export type Root = IRootView;
export type Bind = IBindDecisionView;
export type Dataflow = IDataflowDecisionView;
export type Refactor = IRefactorDecisionView;

export const haskellApiService = {
  getSynthesisTree: (): AxiosPromise<TreeView<SynthesisNodeView>> => api.getSynthesisTree(),

  // Synthesis tree navigation
  getRootPath: (sid: NodeId): AxiosPromise<Node[]> => api.getNodeBySidRootPath(sid),
  getParentEdge: (sid: NodeId): AxiosPromise<Node> => api.getNodeBySidParentEdge(sid),
  getChildEdges: (sid: NodeId): AxiosPromise<Node[]> => api.getNodeBySidSubForest(sid),

  // Synthesis node inspections
  getNode: (sid: NodeId): AxiosPromise<Node> => api.getNodeBySid(sid),
  getIntermediateView: (sid: NodeId): AxiosPromise<IntermediateGraph> => api.getNodeBySidIntermediateView(sid),
  getTimelines: (sid: NodeId): AxiosPromise<any> => api.getNodeBySidProcessTimelines(sid),
  getEndpoints: (sid: NodeId): AxiosPromise<EndpointSts> => api.getNodeBySidEndpoints(sid),
  getDebugInfo: (sid: NodeId): AxiosPromise<any> => api.getNodeBySidDebug(sid),
  runTestBench: (sid: NodeId, name: string, loopsNumber: number): AxiosPromise<TestBenchReport | null> =>
    api.postNodeBySidTestbench(sid, name, loopsNumber),

  // Synthesis methods
  stateOfTheArtSynthesis: (sid: NodeId): AxiosPromise<NodeId> => api.postNodeBySidStateOfTheArtSynthesisIO(sid),
  simpleSynthesis: (sid: NodeId): AxiosPromise<NodeId> => api.postNodeBySidSimpleSynthesis(sid),
  smartBindSynthesisIO: (sid: NodeId): AxiosPromise<NodeId> => api.postNodeBySidSmartBindSynthesisIO(sid),

  // Synthesis practice
  bestStep: (sid: NodeId): AxiosPromise<NodeId> => api.postNodeBySidBestStep(sid),
  allBestThread: (sid: NodeId, n: number): AxiosPromise<NodeId> => api.postNodeBySidAllBestThreads(sid, n),
  obviousBindThread: (sid: NodeId): AxiosPromise<NodeId> => api.postNodeBySidObviousBindThread(sid),
  allBindsAndRefsIO: (sid: NodeId): AxiosPromise<NodeId> => api.postNodeBySidAllBindsAndRefsIO(sid),
};
