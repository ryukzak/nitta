import { CancelTokenSource } from "axios";
import { useEffect } from "react";

function useRequestCancellation(source: CancelTokenSource) {
  useEffect(() => {
    return () => {
      source.cancel();
    };
  }, [source]);
}

export default useRequestCancellation;
