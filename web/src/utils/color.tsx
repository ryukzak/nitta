export class Color {
  /**
   * Constructs color object from corresponding hex string.
   * @param hexString String like "#1f77b4" denoting color
   */
  public static fromHex(hexString: string): Color {
    const strippedHex = hexString.replace("#", "");
    const r = parseInt(strippedHex.substring(0, 2), 16);
    const g = parseInt(strippedHex.substring(2, 4), 16);
    const b = parseInt(strippedHex.substring(4, 6), 16);

    return new Color({ r, g, b });
  }

  constructor(public obj: ColorObject) {
    if (this.obj.a === undefined) {
      this.obj.a = 1;
    }
  }

  public toRgbaString(): string {
    return `rgba(${this.obj.r}, ${this.obj.g}, ${this.obj.b}, ${this.obj.a})`;
  }
}

export interface ColorObject {
  r: number;
  g: number;
  b: number;
  a?: number;
}
