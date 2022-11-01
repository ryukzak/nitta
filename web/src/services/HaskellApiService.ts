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
  Relation,
  TimeConstraint,
  TreeInfo,
  Process,
  Step,
  StepInfoView,
  IAllocationView,
} from "services/gen/types";
import { NodeView, DecisionView, IRootView, IBindDecisionView, IGroupBindDecisionView, IDataflowDecisionView } from "services/gen/types";
import { UnitEndpoints, EndpointSt, ISource, ITarget, GraphStructure, GraphEdge, TestbenchReport } from "services/gen/types";
import { Interval, MicroarchitectureDesc } from "services/gen/types";

export type Sid = string;
export const sidSeparator = "-";
export const reLastSid = /-[^-]*$/;

export type SynthesisTree = TreeView<ShortNodeView>;
export type Node = NodeView<string, string, number, number>;
export type Decision = DecisionView;
export type Root = IRootView;

export type Bind = IBindDecisionView;
export type GroupBind = IGroupBindDecisionView;
export type Allocation = IAllocationView;
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
export type TestBenchReportData = TestbenchReport<string, number>;

export type ProcessData = Process<number, StepInfoView>;
export type StepData = Step<number, StepInfoView>;
export type RelationData = Relation;

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
      .then((response: AxiosResponse<Sid>) => {
        context.setSid(response.data);
      })
      .catch((err: AxiosError) => console.log(err));
  };
}

export const api = {
  getSynthesisTree: (): AxiosPromise<TreeView<ShortNodeView>> => jsAPI.getSynthesisTree(),
  getTreeInfo: (): AxiosPromise<TreeInfo> => jsAPI.getTreeInfo(),

  // Synthesis tree navigation
  getRootPath: (sid: Sid): AxiosPromise<Node[]> => jsAPI.getNodeBySidHistory(sid),
  getParentEdge: (sid: Sid): AxiosPromise<Node> => jsAPI.getNodeBySidParentEdge(sid),
  getSubforest: (sid: Sid): AxiosPromise<Node[]> => jsAPI.getNodeBySidSubForest(sid),

  // Synthesis node inspections
  getNode: (sid: Sid): AxiosPromise<Node> => jsAPI.getNodeBySid(sid),
  getMicroarchitecture: (sid: Sid): AxiosPromise<MicroarchitectureData> => jsAPI.getNodeBySidMicroarchitecture(sid),
  getIntermediateView: (sid: Sid): AxiosPromise<IntermediateGraph> => jsAPI.getNodeBySidIntermediateView(sid),
  getTimelines: (sid: Sid): AxiosPromise<any> => jsAPI.getNodeBySidProcessTimelines(sid),
  getProcess: (sid: Sid): AxiosPromise<ProcessData> => jsAPI.getNodeBySidProcess(sid),
  getEndpoints: (sid: Sid): AxiosPromise<UnitEndpointsData[]> => jsAPI.getNodeBySidEndpoints(sid),
  getDebugInfo: (sid: Sid): AxiosPromise<any> => jsAPI.getNodeBySidDebug(sid),
  runTestBench: (sid: Sid, name: string, loopsNumber: number): AxiosPromise<TestBenchReportData | null> =>
    jsAPI.postNodeBySidTestbench(sid, name, loopsNumber),

  // Synthesis methods
  stateOfTheArtSynthesis: (sid: Sid): AxiosPromise<Sid> => jsAPI.postNodeBySidStateOfTheArtSynthesisIO(sid),
  simpleSynthesis: (sid: Sid): AxiosPromise<Sid> => jsAPI.postNodeBySidSimpleSynthesis(sid),
  smartBindSynthesisIO: (sid: Sid): AxiosPromise<Sid> => jsAPI.postNodeBySidSmartBindSynthesisIO(sid),

  // Synthesis practice
  bestStep: (sid: Sid): AxiosPromise<Sid> => jsAPI.postNodeBySidBestStep(sid),
  allBestThread: (sid: Sid, n: number): AxiosPromise<Sid> => jsAPI.postNodeBySidAllBestThreads(sid, n),
  obviousBindThread: (sid: Sid): AxiosPromise<Sid> => jsAPI.postNodeBySidObviousBindThread(sid),
  allBindsAndRefsIO: (sid: Sid): AxiosPromise<Sid> => jsAPI.postNodeBySidAllBindsAndRefsIO(sid),
};
