export function getFormikFieldClass(name: string, touched: any, errors: any) {
  return `form-control ${touched[name] && errors[name] ? "is-invalid" : ""}`;
}
