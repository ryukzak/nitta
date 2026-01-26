import { ProcessTimelines2 } from "components/ProcessTimeline2";
import React, { FC } from "react";

export type IProcess2ScreenProps = {};

export const Process2Screen: FC<IProcess2ScreenProps> = (props) => {
  return (
    <div>
      <h2 className="p-3">Process Timeline Visualization</h2>
      <ProcessTimelines2 />
    </div>
  );
};
