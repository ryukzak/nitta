import * as React from "react";

export const DownloadTextFile = (props: { name: string; text: string }) => {
  const downloadTextFileCallback = React.useCallback(() => {
    const element = document.createElement("a");
    const file = new Blob([props.text], { type: "text/plain" });
    element.href = URL.createObjectURL(file);
    element.download = props.name;
    document.body.appendChild(element); // Required for this to work in FireFox
    element.click();
  }, [props.name, props.text]);
  return (
    <button className="btn btn-link" onClick={downloadTextFileCallback}>
      <small>download</small>
    </button>
  );
};
