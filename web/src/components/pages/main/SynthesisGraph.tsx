import * as React from "react";
import { Button, Container, Row, Col } from "react-bootstrap";
import AppContext from "../../app/AppContext";

export interface ISynthesisGraphProps {}

export interface ISynthesisGraphState {}

export default class SynthesisGraph extends React.Component<ISynthesisGraphProps, ISynthesisGraphState> {
  static contextType = AppContext;
  context!: React.ContextType<typeof AppContext>;

  constructor(props: ISynthesisGraphProps) {
    super(props);

    this.state = {};
  }

  public render() {
    return (
      <div className="text-center p-5 bg-light border m-2 flex-grow-1">
        <h1 className="text-black-50">SynthesisGraph</h1>
        <Container className="mt-5">
          <Row>
            <Col md={6}>
              <Button onClick={() => this.context.selectNode((this.context.selectedNodeId || 0) + 1)}>
                Test Node Selecting
              </Button>
            </Col>
            <Col md={6} className="m-2 m-md-0">
              <Button onClick={this.context.reloadSelectedNode}>Test Node Resetting</Button>
            </Col>
          </Row>
        </Container>
      </div>
    );
  }
}
