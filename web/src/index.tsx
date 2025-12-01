import { BrowserRouter } from "react-router-dom";
import "./index.scss";
import React from 'react';
import ReactDOM from 'react-dom/client';
import App from 'app/App';

const rootElement = document.getElementById('root');
if (rootElement) {
  ReactDOM.createRoot(rootElement).render(
    <BrowserRouter>
    <App />
  </BrowserRouter>
  );
}
