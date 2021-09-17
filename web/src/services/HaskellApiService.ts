import { AxiosPromise, AxiosResponse, AxiosError, CancelToken } from "axios";
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
} from "services/gen/types";
import { NodeView, DecisionView, IRootView, IBindDecisionView, IDataflowDecisionView } from "services/gen/types";
import {
  UnitEndpoints,
  EndpointSt,
  ISource,
  ITarget,
  GraphStructure,
  GraphEdge,
  TestbenchReport,
} from "services/gen/types";
import { Interval, MicroarchitectureDesc } from "services/gen/types";
import axiosErrorExceptionHandler from "components/utils/axios_errors_handlers/AxiosErrorHander";

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
      .then((response: AxiosResponse<SID>) => {
        context.setSID(response.data);
      })
      .catch((err: AxiosError) => {
        axiosErrorExceptionHandler(err);
      });
  };
}

export const api = {
  getSynthesisTree: (cancelToken: CancelToken): AxiosPromise<TreeView<ShortNodeView>> =>
    jsAPI.getSynthesisTree(cancelToken),
  getTreeInfo: (cancelToken: CancelToken): AxiosPromise<TreeInfo> => jsAPI.getTreeInfo(cancelToken),

  // Synthesis tree navigation
  getRootPath: (sid: SID, cancelToken: CancelToken): AxiosPromise<Node[]> =>
    jsAPI.getNodeBySidHistory(sid, cancelToken),
  getParentEdge: (sid: SID, cancelToken: CancelToken): AxiosPromise<Node> =>
    jsAPI.getNodeBySidParentEdge(sid, cancelToken),
  getSubforest: (sid: SID, cancelToken: CancelToken): AxiosPromise<Node[]> =>
    jsAPI.getNodeBySidSubForest(sid, cancelToken),

  // Synthesis node inspections
  getNode: (sid: SID, cancelToken: CancelToken): AxiosPromise<Node> => jsAPI.getNodeBySid(sid, cancelToken),
  getMicroarchitecture: (sid: SID, cancelToken: CancelToken): AxiosPromise<MicroarchitectureData> =>
    jsAPI.getNodeBySidMicroarchitecture(sid, cancelToken),
  getIntermediateView: (sid: SID, cancelToken: CancelToken): AxiosPromise<IntermediateGraph> =>
    jsAPI.getNodeBySidIntermediateView(sid, cancelToken),
  getTimelines: (sid: SID, cancelToken: CancelToken): AxiosPromise<any> =>
    jsAPI.getNodeBySidProcessTimelines(sid, cancelToken),
  getProcess: (sid: SID, cancelToken: CancelToken): AxiosPromise<ProcessData> =>
    jsAPI.getNodeBySidProcess(sid, cancelToken),
  getEndpoints: (sid: SID, cancelToken: CancelToken): AxiosPromise<UnitEndpointsData[]> =>
    jsAPI.getNodeBySidEndpoints(sid, cancelToken),
  getDebugInfo: (sid: SID, cancelToken: CancelToken): AxiosPromise<any> => jsAPI.getNodeBySidDebug(sid, cancelToken),
  runTestBench: (
    sid: SID,
    cancelToken: CancelToken,
    name: string,
    loopsNumber: number
  ): AxiosPromise<TestBenchReportData | null> => jsAPI.postNodeBySidTestbench(sid, name, loopsNumber, cancelToken),

  // Synthesis methods
  stateOfTheArtSynthesis: (sid: SID, cancelToken: CancelToken): AxiosPromise<SID> =>
    jsAPI.postNodeBySidStateOfTheArtSynthesisIO(sid, cancelToken),
  simpleSynthesis: (sid: SID, cancelToken: CancelToken): AxiosPromise<SID> =>
    jsAPI.postNodeBySidSimpleSynthesis(sid, cancelToken),
  smartBindSynthesisIO: (sid: SID, cancelToken: CancelToken): AxiosPromise<SID> =>
    jsAPI.postNodeBySidSmartBindSynthesisIO(sid, cancelToken),

  // Synthesis practice
  bestStep: (sid: SID, cancelToken: CancelToken): AxiosPromise<SID> => jsAPI.postNodeBySidBestStep(sid, cancelToken),
  allBestThread: (sid: SID, n: number, cancelToken: CancelToken): AxiosPromise<SID> =>
    jsAPI.postNodeBySidAllBestThreads(sid, n, cancelToken),
  obviousBindThread: (sid: SID, cancelToken: CancelToken): AxiosPromise<SID> =>
    jsAPI.postNodeBySidObviousBindThread(sid, cancelToken),
  allBindsAndRefsIO: (sid: SID, cancelToken: CancelToken): AxiosPromise<SID> =>
    jsAPI.postNodeBySidAllBindsAndRefsIO(sid, cancelToken),
};
