import React, { FC } from "react";
import ReactJson, { ReactJsonViewProps } from "react-json-view";

export interface IJsonViewProps extends ReactJsonViewProps {}

const DEFAULT_REACT_JSON_PROPS: Partial<ReactJsonViewProps> = {
  theme: "rjv-default",
  iconStyle: "circle",
  displayDataTypes: false,
  displayObjectSize: true,
  collapsed: 3,
  collapseStringsAfterLength: 120,
};

export const JsonView: FC<IJsonViewProps> = (props) => {
  // default values are overriden by passed props
  return <ReactJson {...{ ...DEFAULT_REACT_JSON_PROPS, ...props }} />;
};
