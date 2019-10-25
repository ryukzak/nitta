import React, { useState, useEffect } from "react";
import { haskellApiService } from "../../../services/HaskellApiService";
import { AppContext, IAppContext } from "../../app/AppContext";
import { useContext } from "react";


export const EndpointOptions: React.FC = () => {
  const appContext = useContext(AppContext) as IAppContext;

  const [endpointOptions, setEndpointOptions] = useState<Array<[string, string]> | null>(null);

  useEffect(() => {
    haskellApiService
      .getEndpointOptions(appContext.selectedNodeId)
      .then((response: any) => {
        console.log("Point : " + response.data);
        setEndpointOptions(response.data);
      })
      .catch((err: any) => console.log(err));
  }, [appContext.selectedNodeId]);


  return <pre> {JSON.stringify(endpointOptions, null, 2)} </pre>;
}
