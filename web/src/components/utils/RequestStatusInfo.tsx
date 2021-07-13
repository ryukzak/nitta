import React, { FC } from "react";
import { Spinner } from "react-bootstrap";
import { ErrorDisplay } from "./ErrorDisplay";
import { RefreshButton, RefreshButtonProps } from "./RefreshButton";

interface RequestStatusInfoProps {
  errorMessage: string | null;
  refreshButtonProps: RefreshButtonProps;
  isSpinnerCentered?: boolean;
}

export const RequestStatusInfo: FC<RequestStatusInfoProps> = (props) => {
  const { errorMessage, refreshButtonProps, isSpinnerCentered = true } = props;

  return (
    <>
      {errorMessage ? (
        <div className="d-flex align-items-stretch w-100">
          <ErrorDisplay message={errorMessage} className="m-0 flex-grow-1" />
          <RefreshButton {...refreshButtonProps} />
        </div>
      ) : (
        <div className={`d-flex ${isSpinnerCentered ? "align-items-center" : ""} flex-column`}>
          <Spinner animation="border" className="font-weight-light d-block mb-4" variant="primary" />
        </div>
      )}
    </>
  );
};
