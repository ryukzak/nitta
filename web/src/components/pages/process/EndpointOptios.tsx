import * as React from "react";
import { haskellApiService } from "../../../services/HaskellApiService";

interface JsonResponse {
  [key: string]: any;
}

export interface IEndpointOptionsProps {
  nId: string;
}

export interface IEndpointOptionsState {
  nId: string | null;
  endpointOptions: JsonResponse | null;
}

export class EndpointOptions extends React.Component<IEndpointOptionsProps, IEndpointOptionsState> {
  constructor(props: IEndpointOptionsProps) {
    super(props);
    this.state = {
      nId: null,
      endpointOptions: {},
    };
    this.updateEndpointOptions(this.state.nId);
  }

  componentWillReceiveProps(props: IEndpointOptionsProps) {
    if (this.state.nId !== props.nId) {
      this.setState({ nId: props.nId });
      this.updateEndpointOptions(this.state.nId!);
    }
  }

  updateEndpointOptions(nid: string | null) {
    haskellApiService
      .getEndpointOptions(nid)
      .then(response => {
        console.log("Point : " + response.data);
        this.setState({
          endpointOptions: response.data,
        });
      })
      .catch((err: any) => console.log(err));
  }

  render() {
    return <pre> {JSON.stringify(this.state.endpointOptions, null, 2)} </pre>;
  }
}
