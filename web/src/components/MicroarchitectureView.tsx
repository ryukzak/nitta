import React, { useContext, useMemo, FC, useCallback } from "react";
import "react-table/react-table.css";
import { Graphviz } from "graphviz-react";

import { AppContext, IAppContext } from "app/AppContext";
import { api, MicroarchitectureData, NetworkData, UnitData } from "services/HaskellApiService";
import { UnitEndpointsData, EndpointOptionData } from "services/HaskellApiService";
import { DownloadTextFile } from "utils/download";

import "components/Graphviz.scss";
import { useApiRequest } from "hooks/useApiRequest";

/**
 * Component to display a microarchitecture with available endpoints.
 */

export interface IMicroarchitectureViewProps {}

export const MicroarchitectureView: FC<IMicroarchitectureViewProps> = (props) => {
  const { selectedSid } = useContext(AppContext) as IAppContext;

  const maRequest = useApiRequest({
    requester: useCallback(() => api.getMicroarchitecture(selectedSid), [selectedSid]),
  });

  const endpointsRequest = useApiRequest({
    requester: useCallback(() => api.getEndpoints(selectedSid), [selectedSid]),
  });

  const dot = useMemo(() => {
    if (maRequest.response && endpointsRequest.response) {
      return renderMicroarchitectureDot(maRequest.response.data, collectEndpoints(endpointsRequest.response.data));
    }
  }, [maRequest.response, endpointsRequest.response]);

  return (
    <div className="bg-light border graphvizContainer">
      {dot && (
        <>
          <Graphviz dot={dot} options={{ height: 399, width: "100%", zoom: true }} />
          <DownloadTextFile name="microarchitecture.dot" text={dot} />
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

function collectEndpoints(data: UnitEndpointsData[]): Endpoints {
  let result: Endpoints = {};
  data.forEach((eps: UnitEndpointsData) => {
    let tag = eps.unitTag;
    result[tag] = { sources: [], targets: [] };
    eps.unitEndpoints.forEach((e: EndpointOptionData) => {
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

function renderMicroarchitectureDot(ma: MicroarchitectureData, endpoints: Endpoints): string {
  var lines: string[] = [];
  var units: string[] = [];
  var vars: string[] = [];

  lines.push("digraph {");
  lines.push("  rankdir=LR;");
  ma.networks.forEach((net: NetworkData) => {
    lines.push(`  ${net.networkTag}[label="${net.networkTag} :: ${net.valueType}"];`);
    net.units.forEach((unit: UnitData) => {
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
