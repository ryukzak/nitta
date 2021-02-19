import { AxiosPromise, AxiosResponse, AxiosError } from "axios";
import { SID, IAppContext } from "components/app/AppContext";

import jsAPI from "gen/rest_api.js";
import { TreeView, ShortNodeView } from "../gen/types";
import { NodeView, DecisionView, IRootView, IBindDecisionView, IDataflowDecisionView } from "../gen/types";
import { EndpointSt, ISource, ITarget, GraphStructure, GraphEdge, TestbenchReportView } from "../gen/types";
import { Interval } from "../gen/types";

export type SynthesisTree = TreeView<ShortNodeView>;
export type Node = NodeView<string, string, number, number>;
export type Decision = DecisionView;
export type Root = IRootView;
export type Bind = IBindDecisionView;
export type Dataflow = IDataflowDecisionView;

export type Endpoint = EndpointSt<string, number>;
export type EndpointDecision = EndpointSt<string, Interval<number>>;
export type Source = ISource<string>;
export type Target = ITarget<string>;
export type PUEndpoints = [string, Endpoint[]];
export type IntermediateGraph = GraphStructure<GraphEdge>;
export type TestBenchReport = TestbenchReportView<string, number>;

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
  getIntermediateView: (sid: SID): AxiosPromise<IntermediateGraph> => jsAPI.getNodeBySidIntermediateView(sid),
  getTimelines: (sid: SID): AxiosPromise<any> => jsAPI.getNodeBySidProcessTimelines(sid),
  getEndpoints: (sid: SID): AxiosPromise<PUEndpoints[]> => jsAPI.getNodeBySidEndpoints(sid),
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
