import d3 from "d3";
import * as Plotly from "plotly.js";
import * as React from "react";
import { Button, Col, Container, Row } from "react-bootstrap";
import PlotlyChart from "react-plotlyjs-ts";
import { Formik, Form } from "formik";
import * as Yup from "yup";
import FormikInputGroup from "../../bricks/forms/FormikInputGroup";

export interface IXYChartPlaygroundProps {}

export interface IXYChartPlaygroundState {
  xs: number[];
  ys: number[];
}

const POINTS_NUM = 200;

function generatePoints(meandy: number, sigma: number): { xs: number[]; ys: number[] } {
  let xs = [0];
  let ys = [0];
  let generator = d3.random.normal(meandy, sigma);

  for (let i = 1; i < POINTS_NUM; i++) {
    xs.push(i);
    ys.push(ys[ys.length - 1] + generator());
  }
  return { xs, ys };
}

const XYChartPlaygroundSchema = Yup.object().shape({
  meandy: Yup.number().required(),
  sigma: Yup.number()
    .required()
    .positive(),
});

export default class XYChartPlayground extends React.Component<IXYChartPlaygroundProps, IXYChartPlaygroundState> {
  constructor(props: IXYChartPlaygroundProps) {
    super(props);

    this.state = {
      xs: [],
      ys: [],
    };
  }

  render() {
    const data: Plotly.Data[] = [
      {
        type: "scatter",
        x: this.state.xs,
        y: this.state.ys,
      },
    ];
    const layout = {
      title: "XY Chart Test",
      xaxis: {
        title: "X Axis Title",
      },
      yaxis: {
        title: "Y Axis Title",
      },
    };

    return (
      <>
        <h3>Regular XY chart</h3>
        <Container fluid={true} className="p-0 mt-3">
          <Row className="align-items-center">
            <Col md={3}>
              <Formik
                initialValues={{
                  meandy: "0",
                  sigma: "1",
                }}
                validationSchema={XYChartPlaygroundSchema}
                onSubmit={(values, { setSubmitting }) => {
                  this.setState({
                    ...generatePoints(parseFloat(values.meandy), parseFloat(values.sigma)),
                  });
                  setSubmitting(false);
                }}
              >
                {({ touched, errors }) => (
                  <Form className="form">
                    <FormikInputGroup
                      type="text"
                      name="meandy"
                      label="Mean dy"
                      notice="Next point's Y increment is generated using this data according to gaussian distribution."
                      {...{ touched, errors }}
                    />
                    <FormikInputGroup
                      type="text"
                      name="sigma"
                      label="Sigma (standard deviation)"
                      {...{ touched, errors }}
                    />
                    <Button variant="primary" type="submit" className="w-100">
                      Generate
                    </Button>
                  </Form>
                )}
              </Formik>
            </Col>
            <Col md={9} className="mt-3 mt-md-0">
              <PlotlyChart {...{ data, layout }}></PlotlyChart>
            </Col>
          </Row>
        </Container>
      </>
    );
  }
}
