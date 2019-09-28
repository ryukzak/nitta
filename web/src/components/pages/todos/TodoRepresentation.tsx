import * as React from "react";
import { Col, Card, Form, Row, Button } from "react-bootstrap";
import Todo from "../../../services/entities/Todo";

export interface ITodoRepresentationProps {
  todo: Todo;
  toggleHandler: (e: React.SyntheticEvent) => void;
  deleteHandler: (e: React.SyntheticEvent) => void;
}

export function TodoRepresentation(props: ITodoRepresentationProps) {
  return (
    <Row className="card card-body flex-row align-items-stretch my-4 mx-1 shadow-sm">
      <Col xs="auto" className="form d-flex align-items-center justify-content-center">
        <Form.Check
          custom
          id={`checkbox-${props.todo.id}`}
          type="checkbox"
          checked={props.todo.completed}
          onChange={props.toggleHandler}
          label=""
        ></Form.Check>
      </Col>
      <Col xs>
        <Card.Title>{props.todo.completed ? <del>{props.todo.title}</del> : props.todo.title}</Card.Title>
        <Card.Subtitle className="text-muted">#{props.todo.id}</Card.Subtitle>
      </Col>
      <Col xs={1} className="d-flex justify-content-center">
        <Button variant="outline-danger" className="border-0" onClick={props.deleteHandler}>
          <span className="h5 mb-0 glyphicon glyphicon-remove"></span>
        </Button>
      </Col>
    </Row>
  );
}
