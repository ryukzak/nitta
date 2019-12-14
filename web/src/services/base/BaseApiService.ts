import HTTPMethod from "http-method-enum";
import FetchError from "./FetchError";
import HttpStatusCodes from "http-status-codes";

export enum RequestContentType {
  JSON = "application/json",
  URLENCODED = "application/x-www-form-urlencoded",
}

export default class BaseApiService {
  constructor(
    private apiBaseUrl: string,
    private requestContentType: RequestContentType = RequestContentType.URLENCODED
  ) {}

  get API_BASE_URL() {
    return this.apiBaseUrl;
  }

  private urlFor(path: string): string {
    return this.apiBaseUrl + path;
  }

  private encodeObjectToUrl(obj: any) {
    let params = [];
    for (let p in obj) {
      if (obj.hasOwnProperty(p)) {
        params.push(encodeURIComponent(p) + "=" + encodeURIComponent(obj[p]));
      }
    }
    return params.join("&");
  }

  private serializeData(requestContentType: RequestContentType, data: any) {
    // BodySerializer for every RequestContentType?
    switch (requestContentType) {
      case RequestContentType.JSON:
        return JSON.stringify(data);
      case RequestContentType.URLENCODED:
        return this.encodeObjectToUrl(data);
    }
  }

  // requestContentType overrides default service-scoped setting passed in constructor
  protected async doRequest<TParams = any, TData = any>(
    method: HTTPMethod,
    path: string,
    params?: TParams,
    data?: TData,
    requestContentType?: RequestContentType
  ) {
    if (requestContentType === undefined) {
      requestContentType = this.requestContentType;
    }

    let response = await fetch(this.urlFor(path), {
      method,
      credentials: "same-origin",
      body: this.serializeData(requestContentType, data),
      headers: {
        "Content-Type": requestContentType,
      },
    });

    if (!response.ok) {
      throw new FetchError(response);
    }

    if (response.status !== HttpStatusCodes.NO_CONTENT) {
      return response.json();
    }
  }

  protected async doGet<TParams = any>(path: string, params?: TParams, requestContentType?: RequestContentType) {
    return this.doRequest(HTTPMethod.GET, path, params, undefined, requestContentType);
  }

  protected async doPost<TParams = any, TData = any>(
    path: string,
    params?: TParams,
    data?: TData,
    requestContentType?: RequestContentType
  ) {
    return this.doRequest(HTTPMethod.POST, path, params, data, requestContentType);
  }

  protected async doPut<TParams = any, TData = any>(
    path: string,
    params?: TParams,
    data?: TData,
    requestContentType?: RequestContentType
  ) {
    return this.doRequest(HTTPMethod.PUT, path, params, data, requestContentType);
  }

  protected async doPatch<TParams = any, TData = any>(
    path: string,
    params?: TParams,
    data?: TData,
    requestContentType?: RequestContentType
  ) {
    return this.doRequest(HTTPMethod.PATCH, path, params, data, requestContentType);
  }

  protected async doDelete<TParams = any>(path: string, params?: TParams, requestContentType?: RequestContentType) {
    return this.doRequest(HTTPMethod.DELETE, path, params, undefined, requestContentType);
  }
}
