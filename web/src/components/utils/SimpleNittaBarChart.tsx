import React from "react";
import ChartComponent from "react-chartjs-2";
import { CHART_COLOR_PALLETE, Color } from "utils/color";

interface NittaBarChartProps {
  data: { [k: string]: number };
  name?: string;
  color?: Color;
}

export const SimpleNittaBarChart: React.FC<NittaBarChartProps> = (props) => {
  const { data: rawData, name = "values", color = CHART_COLOR_PALLETE.blue } = props;

  const chartComponentData = {
    labels: Object.keys(rawData),
    datasets: [
      {
        label: name,
        data: Object.values(rawData),
        backgroundColor: color.toRgbaString(),
      },
    ],
  };

  return <ChartComponent type="bar" data={chartComponentData} />;
};
