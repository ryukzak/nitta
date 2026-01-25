import { BrowserRouter } from "react-router-dom";
import "./index.scss";
import App from "app/App";
import React from "react";
import ReactDOM from "react-dom/client";

const rootElement = document.getElementById("root");
if (rootElement) {
  ReactDOM.createRoot(rootElement).render(
    <BrowserRouter>
      <App />
    </BrowserRouter>,
  );
}
