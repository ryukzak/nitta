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

// https://github.com/mwaskom/seaborn/blob/e04b07eb3df135511e71e556c2bd34ef59ba08ba/seaborn/palettes.py#L19
// "muted" pallete from seaborn
export const CHART_COLOR_PALLETE = {
  blue: Color.fromHex("#4878D0"),
  orange: Color.fromHex("#EE854A"),
  green: Color.fromHex("#6ACC64"),
  red: Color.fromHex("#D65F5F"),
  purple: Color.fromHex("#956CB4"),
  brown: Color.fromHex("#8C613C"),
  pink: Color.fromHex("#DC7EC0"),
  gray: Color.fromHex("#797979"),
  yellow: Color.fromHex("#D5BB67"),
  lightblue: Color.fromHex("#82C6E2"),
};

export const CHART_COLORS = Object.values(CHART_COLOR_PALLETE);
