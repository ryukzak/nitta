import * as React from "react";
import BaseContainerPage from "../BaseContainerPage";
import GanttChartPlayground from "./GanttChartPlayground";
import XYChartPlayground from "./XYChartPlayground";

export interface IGraphPageProps {}

export default class GraphPage extends React.Component<IGraphPageProps> {
  constructor(props: IGraphPageProps) {
    super(props);

    this.state = {};
  }

  render() {
    return (
      <BaseContainerPage
        title="Basic Graphing Test"
        lead="Enjoy plots of automatically generated data. Tweak data generator using given inputs."
      >
        <XYChartPlayground />
        <GanttChartPlayground />
      </BaseContainerPage>
    );
  }
}
