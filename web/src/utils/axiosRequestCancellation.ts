import { CancelTokenSource } from "axios";

export function getCancellingCleanupCallback(source: CancelTokenSource): () => void {
  return () => {
    source.cancel();
  };
}
