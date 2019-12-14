export default class FetchError extends Error {
  constructor(private response: Response) {
    super(`FetchError from ${response.url}: ${response.status} - ${response.statusText}.`);
    Object.setPrototypeOf(this, FetchError.prototype);

    response.text().then(text => {
      console.error(`FetchError occurred with response text: ${text}`);
    });
  }
}
