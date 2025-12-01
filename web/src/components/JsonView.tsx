import React, {FC} from "react";
import JsonViewComponent, {JsonViewProps} from "@uiw/react-json-view";
import {TriangleSolidArrow} from '@uiw/react-json-view/triangle-solid-arrow';

const DEFAULT_REACT_JSON_PROPS: Partial<JsonViewProps<object>> = {
  displayDataTypes: false,
  displayObjectSize: true,
  collapsed: 3,
};

export const JsonView: FC<JsonViewProps<object>> = (props) => {
  // default values are overriden by passed props
  return (
    <JsonViewComponent {...{...DEFAULT_REACT_JSON_PROPS, ...props}}>
      <JsonViewComponent.Arrow>
        <TriangleSolidArrow/>
      </JsonViewComponent.Arrow>
    </JsonViewComponent>
  )
};
