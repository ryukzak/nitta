import * as React from "react";
import { Container, Row, Col } from "react-bootstrap";

export interface IAppFooterProps {}

export const AppFooter: React.FC<IAppFooterProps> = props => {
  return (
    <footer className="bg-dark">
      <Container>
        <Row className="text-center">
          <Col xs={12} md className="d-flex align-items-center mb-4 mb-md-0 p-1">
            <div className="text-light mb-0 w-100 small">© NITTA COLLABORATORS, 2019</div>
          </Col>
        </Row>
      </Container>
    </footer>
  );
};
