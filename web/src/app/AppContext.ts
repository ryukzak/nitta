import React from "react";

import { SID } from "services/HaskellApiService";

export interface IAppContext {
  selectedSID: SID;
  setSID: (sid: SID) => void;
  resetSID: () => void;
}

// exported to use with useContext hook in functional components
export const AppContext = React.createContext<IAppContext | null>(null);

export const AppContextProvider = AppContext.Provider;
export const AppContextConsumer = AppContext.Consumer;
