import { DescToken, DescSymbol } from "../definitions/description";

export function numberDescription(
  x: number,
): DescToken[] {
  return _numberDescription(x, []);
}

function _numberDescription(
  x: number,
  acc: DescToken[],
): DescToken[] {
  const digit = x % 10;
  const next = Math.round((x / 10) - 0.5);
  if (next >= 1) {
    return _numberDescription(next, acc).concat([new DescSymbol(`expl_${digit}.png`)]);
  } else {
    return acc.concat([new DescSymbol(`expl_${digit}.png`)]);
  }
}

export function descSingleton(
  str: string,
): DescToken[] {
  return [new DescSymbol(str)];
}
