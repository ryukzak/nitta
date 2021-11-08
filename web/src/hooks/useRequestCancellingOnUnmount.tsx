import Axios, { CancelTokenSource } from "axios";
import { useEffect, useState } from "react";
import { getCancellingCleanupCallback } from "utils/axiosRequestCancellation";

/**
 * @returns a CancelTokenSource that will be cancelled on component unmount. It's safe to provide
 * it to dependency lists to silence the linter since it will never change.
 */
export function useRequestCancellingOnUnmount(): CancelTokenSource {
  const [source] = useState(Axios.CancelToken.source());

  useEffect(() => {
    return getCancellingCleanupCallback(source);
    // `source` should never change, so the cleanup of this effect will be called only on component unmount
  }, [source]);

  return source;
}
