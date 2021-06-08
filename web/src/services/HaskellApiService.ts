import { AxiosPromise, AxiosResponse, AxiosError } from "axios";
import { IAppContext } from "app/AppContext";

import jsAPI from "services/gen/rest_api";
import {
  TreeView,
  ShortNodeView,
  IBreakLoopView,
  IConstantFoldingView,
  IOptimizeAccumView,
  IResolveDeadlockView,
  NetworkDesc,
  UnitDesc,
  TimeConstraint,
} from "services/gen/types";
import { NodeView, DecisionView, IRootView, IBindDecisionView, IDataflowDecisionView } from "services/gen/types";
import {
  UnitEndpoints,
  EndpointSt,
  ISource,
  ITarget,
  GraphStructure,
  GraphEdge,
  TestbenchReportView,
} from "services/gen/types";
import { Interval, MicroarchitectureDesc } from "services/gen/types";

export type SID = string;
export const sidSeparator = "-";
export const reLastSID = /-[^-]*$/;

export type SynthesisTree = TreeView<ShortNodeView>;
export type Node = NodeView<string, string, number, number>;
export type Decision = DecisionView;
export type Root = IRootView;

export type Bind = IBindDecisionView;
export type Dataflow = IDataflowDecisionView;

export type BreakLoop = IBreakLoopView;
export type ConstantFolding = IConstantFoldingView;
export type OptimizeAccum = IOptimizeAccumView;
export type ResolveDeadlock = IResolveDeadlockView;

export type UnitEndpointsData = UnitEndpoints<string, string, number>;
export type EndpointOptionData = EndpointSt<string, TimeConstraint<number>>;
export type EndpointDecision = EndpointSt<string, Interval<number>>;
export type Source = ISource<string>;
export type Target = ITarget<string>;
export type IntermediateGraph = GraphStructure<GraphEdge>;
export type TestBenchReport = TestbenchReportView<string, number>;

export type MicroarchitectureData = MicroarchitectureDesc<string>;
export type NetworkData = NetworkDesc<string>;
export type UnitData = UnitDesc<string>;

export function synthesize<T extends Array<any>>(
  context: IAppContext,
  requestJob: (...args: T) => AxiosPromise,
  ...args: T
) {
  return () => {
    requestJob(...args)
      .then((response: AxiosResponse<SID>) => {
        context.setSID(response.data);
      })
      .catch((err: AxiosError) => console.log(err));
  };
}

export const api = {
  getSynthesisTree: (): AxiosPromise<TreeView<ShortNodeView>> => jsAPI.getSynthesisTree(),

  // Synthesis tree navigation
  getRootPath: (sid: SID): AxiosPromise<Node[]> => jsAPI.getNodeBySidHistory(sid),
  getParentEdge: (sid: SID): AxiosPromise<Node> => jsAPI.getNodeBySidParentEdge(sid),
  getSubforest: (sid: SID): AxiosPromise<Node[]> => jsAPI.getNodeBySidSubForest(sid),

  // Synthesis node inspections
  getNode: (sid: SID): AxiosPromise<Node> => jsAPI.getNodeBySid(sid),
  getMicroarchitecture: (sid: SID): AxiosPromise<MicroarchitectureData> => jsAPI.getNodeBySidMicroarchitecture(sid),
  getIntermediateView: (sid: SID): AxiosPromise<IntermediateGraph> => jsAPI.getNodeBySidIntermediateView(sid),
  getTimelines: (sid: SID): AxiosPromise<any> => jsAPI.getNodeBySidProcessTimelines(sid),
  getEndpoints: (sid: SID): AxiosPromise<UnitEndpointsData[]> => jsAPI.getNodeBySidEndpoints(sid),
  getDebugInfo: (sid: SID): AxiosPromise<any> => jsAPI.getNodeBySidDebug(sid),
  runTestBench: (sid: SID, name: string, loopsNumber: number): AxiosPromise<TestBenchReport | null> =>
    jsAPI.postNodeBySidTestbench(sid, name, loopsNumber),

  // Synthesis methods
  stateOfTheArtSynthesis: (sid: SID): AxiosPromise<SID> => jsAPI.postNodeBySidStateOfTheArtSynthesisIO(sid),
  simpleSynthesis: (sid: SID): AxiosPromise<SID> => jsAPI.postNodeBySidSimpleSynthesis(sid),
  smartBindSynthesisIO: (sid: SID): AxiosPromise<SID> => jsAPI.postNodeBySidSmartBindSynthesisIO(sid),

  // Synthesis practice
  bestStep: (sid: SID): AxiosPromise<SID> => jsAPI.postNodeBySidBestStep(sid),
  allBestThread: (sid: SID, n: number): AxiosPromise<SID> => jsAPI.postNodeBySidAllBestThreads(sid, n),
  obviousBindThread: (sid: SID): AxiosPromise<SID> => jsAPI.postNodeBySidObviousBindThread(sid),
  allBindsAndRefsIO: (sid: SID): AxiosPromise<SID> => jsAPI.postNodeBySidAllBindsAndRefsIO(sid),
};
