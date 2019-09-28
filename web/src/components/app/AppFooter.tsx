import * as React from "react";
import { Container, Row, Col } from "react-bootstrap";

export interface IAppFooterProps {}

export interface IAppFooterState {}

export default class AppFooter extends React.Component<IAppFooterProps, IAppFooterState> {
  constructor(props: IAppFooterProps) {
    super(props);

    this.state = {};
  }

  public render() {
    return (
      <footer className="bg-dark">
        <Container>
          <Row className="text-center">
            {/* <Col xs={12} md>
              <img src="/footer_logo.svg" height="100" className="mw-100" alt="Footer Logo"></img>
            </Col> */}
            <Col xs={12} md className="d-flex align-items-center mb-4 mb-md-0 p-5">
              <h6 className="text-light mb-0 w-100">© NITTA COLLABORATORS, 2019</h6>
            </Col>
          </Row>
        </Container>
      </footer>
    );
  }
}
