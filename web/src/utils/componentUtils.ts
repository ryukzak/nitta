import { AxiosPromise, AxiosResponse, AxiosError } from "axios";
import { NodeId, IAppContext } from "../components/app/AppContext";

export function requestNidBy<T extends Array<any>>(
  context: IAppContext,
  requestJob: (...args: T) => AxiosPromise,
  ...args: T
) {
  return () => {
    requestJob(...args)
      .then((response: AxiosResponse<NodeId>) => {
        context.selectNode(response.data);
      })
      .catch((err: AxiosError) => console.log(err));
  };
}
