import { AxiosResponse, AxiosError } from "axios";
import * as React from "react";
import "react-table/react-table.css";
import { Graphviz } from "graphviz-react";

import { AppContext, IAppContext } from "components/app/AppContext";
import { api, Microarchitecture } from "services/HaskellApiService";

import "./graphviz.scss";

/**
 * Component to display algorithm graph.
 */

export interface IIntermediateViewProps {}

export const MicroarchitectureView: React.FC<IIntermediateViewProps> = (props) => {
  const { selectedSID } = React.useContext(AppContext) as IAppContext;

  const [dot, setDot] = React.useState<string | null>(null);

  React.useEffect(() => {
    api
      .getMicroarchitecture(selectedSID)
      .then((response: AxiosResponse<Microarchitecture>) => {
        var lines: string[] = [];
        var units: string[] = [];
        var vars: string[] = [];

        lines.push("digraph {");
        lines.push("  rankdir=LR;");
        response.data.networks.forEach((net) => {
          lines.push(`  ${net.networkTag}[label="${net.networkTag} :: ${net.valueType}"];`);
          net.units.forEach((unit) => {
            const name = `${net.networkTag}_${unit.unitTag}`;
            units.push(name);
            lines.push(`  ${name}[label="${unit.unitTag} :: ${unit.unitType}"];`);
            lines.push(`  ${net.networkTag} ->  ${name}[dir="both"];`);
          });
        });

        lines.push(`  { rank = some; ${units.join("; ")} }`);
        lines.push(`  { rank = some; ${vars.join("; ")} }`);
        lines.push("}");

        setDot(lines.join("\n"));
      })
      .catch((err: AxiosError) => console.error(err));
  }, [selectedSID]);

  return (
    <div className="bg-light border graphvizContainer">
      {dot && <Graphviz dot={dot} options={{ height: 399, width: "100%", zoom: true }} />}
    </div>
  );
};
