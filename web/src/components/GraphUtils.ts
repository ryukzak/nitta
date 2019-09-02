export type EdgeId = number;

export interface IEdgeJson {
  from: EdgeId;
  to: EdgeId;
  label: string;
}

export interface INodeJson {
  id: EdgeId;
  label: string;
}

export interface IGraphJson {
  edges: IEdgeJson[];
  nodes: INodeJson[];
}

function isString(obj: any) {
  return typeof obj === "string" || obj instanceof String;
}

function renderDotOptions(options: object) {
  let result = [];

  for (let key in options) {
    let representation = (isString(options[key])) ? `"${options[key]}"` : options[key];
    result.push(`${key}=${representation}`);
  }

  return `[${result.join("; ")}]`;
}

export function renderGraphJsonToDot(json: IGraphJson): string {
  let lines = [
    // "rankdir=LR"
  ];

  lines.push(...json.nodes.map(
    node => node.id + " " + renderDotOptions({ label: node.label })
  ));

  lines.push(...json.edges.map(
    edge => `${edge.from} -> ${edge.to} ` + renderDotOptions({ label: edge.label })
  ));

  const wrap = (content: string) => `\t${content};`;
  let result = `digraph {\n${lines.map(wrap).join("\n")}\n}`;
  console.log(result);
  return result;
}

