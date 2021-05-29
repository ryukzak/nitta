import React from "react";
import { Alert } from "react-bootstrap";

interface ErrorDisplayProps extends React.HTMLAttributes<HTMLElement> {
  message: string;
}

export const ErrorDisplay: React.FC<ErrorDisplayProps> = props => {
  const { message, ...rest } = props;

  return (
    <Alert variant="danger" {...rest}>
      {message}
    </Alert>
  );
};
