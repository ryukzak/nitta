import { NId } from "./types";

// EdgesView: table text column style
// SynthesisGraphView: array of Graph attributes data
export interface JsonResponse {
  [key: string]: any;
}

// SynthesisGraphView: array of Graph data by nid (nIds)
export interface JsonObjId {
  [key: string]: Graph;
}

// SynthesisGraphView: type for react-d3-tree lib data
export interface Graph {
  name?: string;
  nid?: NId;
  attributes?: JsonResponse;
  status?: boolean;
  children?: Graph[];
  nodeSvgShape?: any;
  nodeSvgShapeOriginal?: any;
}
