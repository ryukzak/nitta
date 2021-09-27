import Axios from "axios";

export const axiosErrorExceptionHandler = (err: Error) => {
  if (!Axios.isCancel(err)) {
    console.log(err);
  }
};

export const axiosErrorHandlerWithCallback = (err: Error, callback: () => void) => {
  if (!Axios.isCancel(err)) {
    console.log(err);
    callback();
  }
};
