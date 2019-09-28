import { ErrorMessage, Field, Form, Formik } from "formik";
import * as React from "react";
import { Button, Col } from "react-bootstrap";
import * as Yup from "yup";
import Todo from "../../../services/entities/Todo";
import todo_backend from "../../../services/TodoApiService";
import { getFormikFieldClass } from "../../../utils/componentUtils";

export interface ITodosWriterProps {
  newTodoHandler: (todo: Todo) => void;
}

export interface ITodosWriterState {}

const TodosWriterSchema = Yup.object().shape({
  title: Yup.string()
    .required()
    .max(50),
});

export default class TodosWriter extends React.Component<ITodosWriterProps, ITodosWriterState> {
  constructor(props: ITodosWriterProps) {
    super(props);

    this.state = {};
  }

  render() {
    return (
      <Formik
        initialValues={{ title: "" }}
        validationSchema={TodosWriterSchema}
        onSubmit={(values, { setSubmitting, resetForm }) => {
          todo_backend.postTodo(values.title).then(resp => {
            this.props.newTodoHandler(resp);
            resetForm();
            setSubmitting(false);
          });
        }}
      >
        {({ touched, errors, isSubmitting }) => (
          <Form noValidate className="mb-5 form-row">
            <Col md className="form-group">
              <Field
                type="text"
                name="title"
                className={getFormikFieldClass("title", touched, errors)}
                disabled={isSubmitting}
              />
              <ErrorMessage name="title" component="div" className="invalid-feedback" />
            </Col>
            <Col md="auto">
              <Button variant="primary" type="submit" className="w-100" disabled={isSubmitting}>
                Add
              </Button>
            </Col>
          </Form>
        )}
      </Formik>
    );
  }
}
