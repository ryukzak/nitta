import { AxiosPromise, AxiosResponse, AxiosError } from "axios";
import { SelectedNodeId, IAppContext } from "../components/app/AppContext";

export function getFormikFieldClass(name: string, touched: any, errors: any) {
  return `form-control ${touched[name] && errors[name] ? "is-invalid" : ""}`;
}

export function requestNidBy<T extends Array<any>>(
  context: IAppContext,
  requestJob: (...args: T) => AxiosPromise,
  ...args: T
) {
  return () => {
    requestJob(...args)
      .then((response: AxiosResponse<SelectedNodeId>) => {
        context.selectNode(response.data);
      })
      .catch((err: AxiosError) => console.log(err));
  };
}
