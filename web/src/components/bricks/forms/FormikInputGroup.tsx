import * as React from "react";
import { Field, ErrorMessage } from "formik";
import { getFormikFieldClass } from "../../../utils/componentUtils";

export interface IFormikInputGroupProps {
  touched: any;
  errors: any;
  name: string;
  notice?: string;
  label?: string;
  type: string;
}

export interface IFormikInputGroupState {}

export default class FormikInputGroup extends React.Component<IFormikInputGroupProps, IFormikInputGroupState> {
  constructor(props: IFormikInputGroupProps) {
    super(props);

    this.state = {};
  }

  public render() {
    return (
      <div className="form-group">
        {this.props.label && <label htmlFor={this.props.name}>{this.props.label}</label>}
        <Field
          type={this.props.type}
          className={getFormikFieldClass(this.props.name, this.props.touched, this.props.errors)}
          name={this.props.name}
        />
        {this.props.notice && (!this.props.touched.meandy || !this.props.errors.meandy) && (
          <small className="text-muted form-text"> {this.props.notice} </small>
        )}
        <ErrorMessage name={this.props.name} component="div" className="invalid-feedback" />
      </div>
    );
  }
}
