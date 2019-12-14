import * as React from "react";
import { Container, Row, Col } from "react-bootstrap";

export interface IBaseContainerPageProps {
  title: string;
  lead?: string;
}

export interface IBaseContainerPageState {}

export default class BaseContainerPage extends React.Component<IBaseContainerPageProps, IBaseContainerPageState> {
  constructor(props: IBaseContainerPageProps) {
    super(props);

    this.state = {};
  }

  public render() {
    return (
      <Container className="pb-5">
        <Row>
          <Col>
            <h1 className="display-4 mt-4">{this.props.title}</h1>
            <p className="lead">{this.props.lead}</p>
            <hr className="mb-5" />
          </Col>
        </Row>
        {this.props.children}
      </Container>
    );
  }
}
