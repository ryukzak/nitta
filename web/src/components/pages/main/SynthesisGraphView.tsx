import * as React from "react";
import { Container } from "react-bootstrap";
import AppContext from "../../app/AppContext";

export interface ISynthesisGraphProps { }

export interface ISynthesisGraphState { }


export default class SynthesisGraph extends React.Component<ISynthesisGraphProps, ISynthesisGraphState> {
  static contextType = AppContext;
  context!: React.ContextType<typeof AppContext>;
  private myRef = React.createRef<HTMLDivElement>()

  constructor(props: ISynthesisGraphProps) {
    super(props);

    this.state = {}
  }

  componentDidMount() {
    if (this.myRef.current != null && this.context.synthesisGraphHeight === 0) {
      this.context.changedSynthesisGraphHeight((this.context.synthesisGraphHeight = this.myRef.current.clientHeight));
      this.context.minSynthesisGraphHeight = this.context.synthesisGraphHeight;
    }
  }

  public render() {
    return (
      <div className="text-center p-5 bg-light border m-2 flex-grow-1" ref={this.myRef} style={{height: this.context.synthesisGraphHeight}}>
        <h1 className="text-black-50">SynthesisGraph</h1>
        <Container className="mt-5">
          <h1>{this.context.synthesisGraphHeight}</h1>
        </Container>
      </div>
    );
  }
}
