import HttpStatus from "http-status-codes";
import * as React from "react";
import BaseErrorPage from "./BaseErrorPage";

export interface INotFoundErrorPageProps {}

export default function NotFoundErrorPage(props: INotFoundErrorPageProps) {
  return (
    <BaseErrorPage
      httpCode={HttpStatus.NOT_FOUND}
      title="We can't find the page you're looking for."
      description="It may have been moved or deleted. Sorry about that."
    ></BaseErrorPage>
  );
}
