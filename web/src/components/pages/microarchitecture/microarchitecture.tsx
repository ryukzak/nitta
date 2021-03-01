import { AxiosResponse, AxiosError } from "axios";
import * as React from "react";
import "react-table/react-table.css";
import { Graphviz } from "graphviz-react";

import { AppContext, IAppContext } from "components/app/AppContext";
import { api, Microarchitecture } from "services/HaskellApiService";
import { PUEndpoints } from "services/HaskellApiService";

import "./graphviz.scss";

/**
 * Component to display algorithm graph.
 */

export interface IIntermediateViewProps {}

export const MicroarchitectureView: React.FC<IIntermediateViewProps> = (props) => {
  const { selectedSID } = React.useContext(AppContext) as IAppContext;

  const [ma, setMA] = React.useState<Microarchitecture | null>(null);
  const [endpoints, setEndpoints] = React.useState<Endpoints | null>(null);

  React.useEffect(() => {
    api
      .getMicroarchitecture(selectedSID)
      .then((response: AxiosResponse<Microarchitecture>) => setMA(response.data))
      .catch((err: AxiosError) => console.error(err));

    api
      .getEndpoints(selectedSID)
      .then((response: AxiosResponse<PUEndpoints[]>) => setEndpoints(collectEndpoints(response.data)))
      .catch((err: AxiosError) => console.error(err));
  }, [selectedSID]);

  return (
    <div className="bg-light border graphvizContainer">
      {ma && endpoints && (
        <Graphviz
          dot={renderMicroarchitectureDot(ma, endpoints)}
          options={{ height: 399, width: "100%", zoom: true }}
        />
      )}
    </div>
  );
};

type Endpoints = {
  [tag: string]: {
    sources: string[];
    targets: string[];
  };
};

function collectEndpoints(data: PUEndpoints[]): Endpoints {
  let result: Endpoints = {};
  data.forEach((eps: PUEndpoints) => {
    const [tag, endpoints] = eps;
    result[tag] = { sources: [], targets: [] };
    endpoints.forEach((e) => {
      let role = e.epRole;
      if (role.tag === "Source") {
        result[tag].sources.push(...role.contents);
      }
      if (role.tag === "Target") {
        result[tag].targets.push(role.contents);
      }
    });
  });
  return result;
}

function renderMicroarchitectureDot(ma: Microarchitecture, endpoints: Endpoints): string {
  var lines: string[] = [];
  var units: string[] = [];
  var vars: string[] = [];

  lines.push("digraph {");
  lines.push("  rankdir=LR;");
  ma.networks.forEach((net) => {
    lines.push(`  ${net.networkTag}[label="${net.networkTag} :: ${net.valueType}"];`);
    net.units.forEach((unit) => {
      const name = `${net.networkTag}_${unit.unitTag}`;
      units.push(name);
      lines.push(`  ${name}[label="${unit.unitTag} :: ${unit.unitType}"];`);
      lines.push(`  ${net.networkTag} -> ${name}[dir="both"];`);

      endpoints[unit.unitTag].sources.forEach((e) => {
        lines.push(`  ${name} -> "${e}";`);
        vars.push(`"` + e + `"`);
      });
      endpoints[unit.unitTag].targets.forEach((e) => {
        lines.push(`  "${e}" -> ${name};`);
        vars.push(`"` + e + `"`);
      });
    });
  });

  lines.push(`  { rank = some; ${units.join("; ")} }`);
  lines.push(`  { rank = max; ${vars.join("; ")} }`);
  lines.push("}");

  console.log(lines.join("\n"));
  return lines.join("\n");
}
