import { ViewPointID } from "../gen/types";


export function getFormikFieldClass(name: string, touched: any, errors: any) {
  return `form-control ${touched[name] && errors[name] ? "is-invalid" : ""}`;
}

export function viewpoint2string(view: ViewPointID): string {
  return view.component + "@" + view.level;
}
