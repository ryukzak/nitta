import { AxiosResponse } from "axios";
import { useCallback, useEffect, useState } from "react";

interface UseApiRequestArgs<TResult> {
  requester: () => Promise<AxiosResponse<TResult>>;
}

export interface UseApiRequestResult<TResult> {
  response: AxiosResponse<TResult> | null;
  errorMessage: string | null;
  refreshRequestData: () => void;
}

export function useApiRequest<TResult>(args: UseApiRequestArgs<TResult>): UseApiRequestResult<TResult> {
  const [response, setResponse] = useState<AxiosResponse<TResult> | null>(null);
  const [errorMessage, setErrorMessage] = useState<string | null>(null);

  const doRequest = useCallback(() => {
    setErrorMessage(null);
    setResponse(null);

    // workaround for static deps checker
    // it doesn't see that only requester is used otherwise, produces warning
    const requester = args.requester;
    requester()
      .then((response: AxiosResponse<TResult>) => {
        // additional callbacks can be added here or on .catch if needed
        // (accept function via args and call it here, beware of deps)
        setResponse(response);
      })
      .catch((err: Error) => {
        console.error(err);
        setErrorMessage(`Request failed: ${err.message}`);
      });
  }, [args.requester]);

  useEffect(() => {
    doRequest();
  }, [doRequest]);

  return {
    response,
    errorMessage,
    refreshRequestData: doRequest,
  };
}
