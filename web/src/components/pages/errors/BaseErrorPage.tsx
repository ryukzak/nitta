import { getStatusText } from "http-status-codes";
import * as React from "react";
import BaseContainerPage from "../BaseContainerPage";

export interface IBaseErrorPageProps {
  httpCode: number;
  description?: string;
  title?: string;
}

export interface IBaseErrorPageState {}

// Children of this page should be in <Row>'s, since it's actually a container.
export default class BaseErrorPage extends React.Component<IBaseErrorPageProps, IBaseErrorPageState> {
  constructor(props: IBaseErrorPageProps) {
    super(props);

    this.state = {};
  }

  public render() {
    return (
      <BaseContainerPage
        title={this.props.title || "A error has occurred!"}
        lead={this.props.description || `Error ${this.props.httpCode} - ${getStatusText(this.props.httpCode)}`}
      >
        {this.props.children}
      </BaseContainerPage>
    );
  }
}
