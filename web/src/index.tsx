import React from "react";
import ReactDOM from "react-dom";
import "./index.scss";
import App from "./components/app/App";
import { BrowserRouter } from "react-router-dom";

ReactDOM.render(
  <BrowserRouter>
    <App />
  </BrowserRouter>,
  document.getElementById("root")
);
