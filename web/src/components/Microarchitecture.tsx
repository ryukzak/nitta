import { AxiosResponse, AxiosError } from "axios";
import * as React from "react";
import "react-table/react-table.css";
import { Graphviz } from "graphviz-react";

import { AppContext, IAppContext } from "app/AppContext";
import { api, Microarchitecture, Network, Unit } from "services/HaskellApiService";
import { PUEndpoints } from "services/HaskellApiService";
import { DownloadTextFile } from "utils/download";

import "components/Graphviz.scss";

/**
 * Component to display algorithm graph.
 */

export interface IMicroarchitectureViewProps {}

export const MicroarchitectureView: React.FC<IMicroarchitectureViewProps> = (props) => {
  const { selectedSID } = React.useContext(AppContext) as IAppContext;

  const [ma, setMA] = React.useState<Microarchitecture | null>(null);
  const [endpoints, setEndpoints] = React.useState<Endpoints | null>(null);

  React.useEffect(() => {
    setMA(null);
    setEndpoints(null);
    api
      .getMicroarchitecture(selectedSID)
      .then((response: AxiosResponse<Microarchitecture>) => setMA(response.data))
      .catch((err: AxiosError) => console.error(err));
    api
      .getEndpoints(selectedSID)
      .then((response: AxiosResponse<PUEndpoints[]>) => setEndpoints(collectEndpoints(response.data)))
      .catch((err: AxiosError) => console.error(err));
  }, [selectedSID]);

  const dot = React.useMemo(() => {
    if (ma && endpoints) {
      return renderMicroarchitectureDot(ma, endpoints);
    }
  }, [ma, endpoints]);

  return (
    <div className="bg-light border graphvizContainer">
      {dot && (
        <>
          <Graphviz dot={dot} options={{ height: 399, width: "100%", zoom: true }} />
          <DownloadTextFile name={"microarchitecture.dot"} text={dot} />
        </>
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
  data.forEach(([tag, endpoints]: PUEndpoints) => {
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
  ma.networks.forEach((net: Network) => {
    lines.push(`  ${net.networkTag}[label="${net.networkTag} :: ${net.valueType}"];`);
    net.units.forEach((unit: Unit) => {
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

  return lines.join("\n");
}
