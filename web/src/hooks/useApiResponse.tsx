import { useState, useEffect } from "react";
import { UseApiRequestResult } from "./useApiRequest";

export function useApiResponse<TResponseResult, TResult>(
  { response }: UseApiRequestResult<TResponseResult>,
  toResult: (result: TResponseResult) => TResult,
  defaultResult: TResult
): TResult {
  const [result, setResult] = useState(defaultResult);
  useEffect(() => {
    if (!response) {
      setResult(defaultResult);
      return;
    }
    setResult(toResult(response.data));
  }, [response, defaultResult, toResult]);
  return result;
}
