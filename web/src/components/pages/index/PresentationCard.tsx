import * as React from "react";
import { Col } from "react-bootstrap";

export interface IPresentationCardProps {
  icon: JSX.Element;
  title: string;
  content?: string;
}

export function PresentationCard(props: IPresentationCardProps) {
  return (
    <Col sm={12} md={6} lg className="mt-5 text-center">
      <div className="h1">{props.icon}</div>
      <h5>{props.title}</h5>
      <p>{props.content}</p>
    </Col>
  );
}
