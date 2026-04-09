import { ProcessTimelines2 } from "components/ProcessTimeline2";
import React, { type FC } from "react";

export type IProcess2ScreenProps = {};

export const Process2Screen: FC<IProcess2ScreenProps> = (props) => {
  return (
    <div
      style={{
        backgroundColor: "white",
        display: "flex",
        flexDirection: "column",
      }}
    >
      <h3 className="p-3">Process Timeline Visualization</h3>
      <ProcessTimelines2 />
    </div>
  );
};
