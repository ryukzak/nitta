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

  public toHexString(): string {
    const toHex = (value: number): string =>
      Math.max(0, Math.min(255, value)).toString(16).padStart(2, "0");

    const a = this.obj.a ? this.obj.a : 1;
    return `#${toHex(this.obj.r)}${toHex(this.obj.g)}${toHex(this.obj.b)}${toHex(a * 255)}`;
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

export const COMPONENT_COLORS: Record<string, Color> = {
  default: Color.fromHex("#667eea"),
  red: Color.fromHex("#f56565"),
  orange: Color.fromHex("#F38A3F"),
  yellow: Color.fromHex("#E3BC1E"),
  green: Color.fromHex("#2ABB7F"),
  cyan: Color.fromHex("#38b2ac"),
  blue: Color.fromHex("#388BFF"),
  purple: Color.fromHex("#8F7EE7"),
  pink: Color.fromHex("#DA62AC"),
};

export const CHART_COLORS = Object.values(CHART_COLOR_PALLETE);

export function fadeColor(color: Color, opacity: number): Color {
  /**
   * Applies fade to hex color but with alpha = 1
   * @param color Color obhj
   * @param opacity Number from 0 to 1 (divide by 255)
   */
  const faded = (i: number) => Math.round(i * opacity + 255 * (1 - opacity));
  return new Color({
    r: faded(color.obj.r),
    g: faded(color.obj.g),
    b: faded(color.obj.b),
    a: 1,
  });
}
