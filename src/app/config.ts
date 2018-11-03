export const config = {
  gameWidth: 3840,
  gameHeight: 2160,
};

export function fromTop(
  x: number,
) {
  return x;
}

export function fromBottom(
  x: number,
) {
  return config.gameHeight - x;
}

export function fromLeft(
  x: number,
) {
  return x;
}

export function fromRight(
  x: number,
) {
  return config.gameWidth - x;
}