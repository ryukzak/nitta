import React from "react";
import { render, fireEvent, screen, getAllByRole, getAllByTestId, act, waitFor} from "@testing-library/react";

import App from '../app/App'
import { BrowserRouter } from "react-router-dom";
import ReactDOM from "react-dom";

let container: HTMLDivElement | null;

beforeAll(() => {
  container = document.createElement('div');
  document.body.appendChild(container);
});

// afterEach(() => {
//   if (container != null) {
//     document.body.removeChild(container);
//   }
//   container = null;
// });

describe("<App />", () => {
  test("should follow basic workflow without errors", async () => {
    const { exec } = require('child_process');

    exec('yarn backstart', (err: any, stdout: any, stderr: any) => {});
     
     await act(async () => {
        await ReactDOM.render(
        <BrowserRouter>
          <App />
        </BrowserRouter>,
        container
        )
    })

    const forwardButton = screen.getByTestId("forward-button");

    expect(forwardButton.textContent).toBe("Forward");

   
    // const subforestButton = screen.getByTestId("subforest-testid");
    // act(() => {
    //   fireEvent.click(subforestButton);

    // });

    // const forwardButton = screen.getByTestId("forward-button");
    // for (let i = 0; i < 1000; i++) {
    //   act(() => {
    //     fireEvent.click(forwardButton);
    //   });
    // }


    // expect(appEl.textContent).toBe("404");
    
  });
});