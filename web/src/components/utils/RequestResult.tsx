import { RequestStatusInfo } from "components/utils/RequestStatusInfo";
import type React from "react";

export interface NoResultRendererData {
  errorMessage: string | null;
  refreshRequestData: () => void;
}

interface RequestResultProps<TResult> {
  result: TResult | null;
  resultRenderer: (result: TResult) => React.JSX.Element;
  noResultRenderer: (() => React.JSX.Element) | NoResultRendererData;
}

export function RequestResult<TResult>(props: RequestResultProps<TResult>) {
  const { result, resultRenderer, noResultRenderer } = props;

  if (result) {
    return resultRenderer(result);
  }

  if (typeof noResultRenderer === "function") {
    return noResultRenderer();
  }

  return (
    <div className="my-5">
      <RequestStatusInfo
        errorMessage={noResultRenderer.errorMessage}
        refreshButtonProps={{ onClick: noResultRenderer.refreshRequestData }}
        isSpinnerCentered={false}
      />
    </div>
  );
}
