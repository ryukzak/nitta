import { AppContext, type IAppContext } from "app/AppContext";
import { Graphviz } from "graphviz-react";
import { type FC, useCallback, useContext, useMemo } from "react";
import {
  api,
  type EndpointOptionData,
  type MicroarchitectureData,
  type NetworkData,
  type UnitData,
  type UnitEndpointsData,
} from "services/HaskellApiService";
import { DownloadTextFile } from "utils/download";
import "components/Graphviz.scss";
import { useApiRequest } from "hooks/useApiRequest";
import { COMPONENT_COLORS, Color, fadeColor } from "../utils/color";

/**
 * Component to display a microarchitecture with available endpoints.
 */

export type DataFlow = {
  source: string;
  targetsWithVars: Map<string, string>;
  net: string;
};

export type IMicroarchitectureViewProps = {
  unitColors?: Map<string, string>;
  enabledUnits?: Set<string>;
  highligthedUnits?: Set<string>;
  highlightedDataFlows?: Set<DataFlow>;
};

export const MicroarchitectureView: FC<IMicroarchitectureViewProps> = (
  _props,
) => {
  const { selectedSid } = useContext(AppContext) as IAppContext;

  const maRequest = useApiRequest({
    requester: useCallback(
      () => api.getMicroarchitecture(selectedSid),
      [selectedSid],
    ),
  });

  const endpointsRequest = useApiRequest({
    requester: useCallback(() => api.getEndpoints(selectedSid), [selectedSid]),
  });

  const dot = useMemo(() => {
    if (maRequest.response && endpointsRequest.response) {
      return renderMicroarchitectureDot(
        maRequest.response.data,
        collectEndpoints(endpointsRequest.response.data),
        _props.unitColors,
        _props.enabledUnits,
        _props.highligthedUnits,
        _props.highlightedDataFlows,
      );
    }
  }, [
    maRequest.response,
    endpointsRequest.response,
    _props.unitColors,
    _props.enabledUnits,
    _props.highligthedUnits,
    _props.highlightedDataFlows,
  ]);

  return (
    <div className="bg-light border graphvizContainer">
      {dot && (
        <>
          <Graphviz
            dot={dot}
            options={{ height: 399, width: "100%", zoom: true }}
          />
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
  const result: Endpoints = {};
  data.forEach((eps: UnitEndpointsData) => {
    const tag = eps.unitTag;
    result[tag] = { sources: [], targets: [] };
    eps.unitEndpoints.forEach((e: EndpointOptionData) => {
      const role = e.epRole;
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

function renderMicroarchitectureDot(
  ma: MicroarchitectureData,
  endpoints: Endpoints,
  unitColors?: Map<string, string>,
  enabledUnits?: Set<string>,
  highligthedUnits?: Set<string>,
  highlightedDataFlows?: Set<DataFlow>,
): string {
  const lines: string[] = [];
  const units: string[] = [];
  const vars: string[] = [];

  lines.push("digraph {");
  lines.push("  rankdir=LR;");
  ma.networks.forEach((net: NetworkData) => {
    let networkProperties = `[label="${net.networkTag} :: ${net.valueType}"]`;
    if (
      highlightedDataFlows &&
      Array.from(highlightedDataFlows).some((df) => df.net === net.networkTag)
    ) {
      networkProperties = `[label="${net.networkTag} :: ${net.valueType}";penwidth=3]`;
    }
    lines.push(`  ${net.networkTag}${networkProperties};`);
    net.units.forEach((unit: UnitData) => {
      const name = `${net.networkTag}_${unit.unitTag}`;
      units.push(name);
      if (unitColors && unitColors.has(unit.unitTag)) {
        const colorKey = unitColors.get(unit.unitTag)!;
        let color = COMPONENT_COLORS[colorKey as keyof typeof COMPONENT_COLORS];
        let fontColor = color;
        let fadedColor = fadeColor(color, 0x15 / 255);

        if (!enabledUnits?.has(unit.unitTag)) {
          color = new Color({
            r: color.obj.r,
            g: color.obj.g,
            b: color.obj.b,
            a: 0.4,
          });
          fadedColor = new Color({
            r: fadedColor.obj.r,
            g: fadedColor.obj.g,
            b: fadedColor.obj.b,
            a: 0.4,
          });
          fontColor = fadeColor(color, 0.4);
        }

        let colorProperties = `color="${color.toHexString()}"; style="filled"; fillcolor="${fadedColor.toHexString()}"; fontcolor="${fontColor.toHexString()}"; tooltip="${name}"`;
        if (highligthedUnits && highligthedUnits.has(unit.unitTag)) {
          colorProperties = `${colorProperties}; penwidth=3;`;
        }

        lines.push(
          `  ${name}[label="${unit.unitTag} :: ${unit.unitType}"; ${colorProperties}];`,
        );
      } else {
        lines.push(`  ${name}[label="${unit.unitTag} :: ${unit.unitType}"];`);
      }

      let connectionProperties = `[arrowhead="none"; arrowtail="none"]`;

      if (highlightedDataFlows) {
        highlightedDataFlows.forEach((df) => {
          if (df.source === unit.unitTag) {
            const vars = Array.from(df.targetsWithVars.values()).join(", ");
            connectionProperties = `[label="${vars}";arrowhead="none";dir="both";arrowtail="";penwidth=3]`;
          }
          if (df.targetsWithVars.has(unit.unitTag)) {
            connectionProperties = `[label="${df.targetsWithVars.get(unit.unitTag)}";arrowhead="";arrowtail="none";penwidth=3]`;
          }
        });
      }

      lines.push(`  ${net.networkTag} -> ${name}${connectionProperties};`);

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
