import Axios from "axios";

const axiosErrorExceptionHandler = (err: Error) => {
    if (!Axios.isCancel(err)) {
        console.log(err);
      }
}

export default axiosErrorExceptionHandler;