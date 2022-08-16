import React from "react";

import { Sid } from "services/HaskellApiService";

export interface IAppContext {
  selectedSid: Sid;
  setSid: (sid: Sid) => void;
  resetSid: () => void;
}

// exported to use with useContext hook in functional components
export const AppContext = React.createContext<IAppContext | null>(null);

export const AppContextProvider = AppContext.Provider;
export const AppContextConsumer = AppContext.Consumer;
