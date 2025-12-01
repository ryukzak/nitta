import React from "react";
import {
  Chart as ChartJS,
  CategoryScale,
  LinearScale,
  BarElement,
  Title,
  Tooltip,
  Legend,
} from 'chart.js';
import { Chart } from "react-chartjs-2";
import { CHART_COLOR_PALLETE, Color } from "utils/color";

ChartJS.register(
  CategoryScale,
  LinearScale,
  BarElement,
  Title,
  Tooltip,
  Legend
);

interface MapHistogramProps {
  data: { [k: string]: number };
  name?: string;
  color?: Color;
}

export const MapHistogram: React.FC<MapHistogramProps> = (props) => {
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

  return <Chart type="bar" data={chartComponentData} />;
};
