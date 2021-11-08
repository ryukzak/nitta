import Axios, { AxiosError } from "axios";

export const getDefaultAxiosErrorHandler = (callback?: (err: Error | AxiosError) => void) => {
  return (err: Error) => {
    if (!Axios.isCancel(err)) {
      console.error("Error on performing request: ", err);
      if (callback) {
        callback(err);
      }
    }
  };
};
