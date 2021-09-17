import Axios from "axios";

const axiosErrorHandlerWithCallback = (err: Error, callback: () => void) => {
    if (!Axios.isCancel(err)) {
        console.log(err);
        callback();
      }
}

export default axiosErrorHandlerWithCallback;