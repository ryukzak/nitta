import * as React from "react";

export const DownloadTextFile = (props: { name: string; text: string }) => {
  const downloadTextFileCallback = (name: string, text: string) => () => {
    const element = document.createElement("a");
    const file = new Blob([text], { type: "text/plain" });
    element.href = URL.createObjectURL(file);
    element.download = name;
    document.body.appendChild(element); // Required for this to work in FireFox
    element.click();
  };
  return (
    <button className="btn btn-link" onClick={downloadTextFileCallback(props.name, props.text)}>
      <small>download</small>
    </button>
  );
};
