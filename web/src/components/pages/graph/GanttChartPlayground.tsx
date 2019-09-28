import * as React from "react";
import PlotlyChart from "react-plotlyjs-ts";
import { GANTT_EXAMPLE } from "./ganttExample";

export interface IGanttChartPlaygroundProps {}

export interface IGanttChartPlaygroundState {}

export default class GanttChartPlayground extends React.Component<
  IGanttChartPlaygroundProps,
  IGanttChartPlaygroundState
> {
  constructor(props: IGanttChartPlaygroundProps) {
    super(props);

    this.state = {};
  }

  render() {
    return (
      <div className="mt-3">
        <h3>Gantt Chart</h3>
        <PlotlyChart {...GANTT_EXAMPLE}></PlotlyChart>
      </div>
    );
  }
}
