import React, { ButtonHTMLAttributes, ReactElement } from "react";
import { Button } from "react-bootstrap";

export type RefreshButtonProps = ButtonHTMLAttributes<HTMLButtonElement>;

export const RefreshButton = (props: RefreshButtonProps): ReactElement => {
  const { className, ...restProps } = props;

  return (
    <Button className={`${className || ""} px-3 ml-3`} variant="secondary" {...restProps}>
      <span className="h6 mb-0 glyphicon glyphicon-refresh" />
    </Button>
  );
};
