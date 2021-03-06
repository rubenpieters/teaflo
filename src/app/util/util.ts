export function filterUndefined<A>(
  l: (A | undefined)[],
): A[] {
  return <A[]>l.filter(x => x !== undefined);
}

export function repeat<A>(
  x: number,
  a: A,
): A[] {
  const l: A[] = [];
  for (let i = 0; i < x; i++) {
    l.push(a);
  }
  return l;
}

export function range0(
  endExclusive: number,
): number[] {
  return Array(endExclusive).fill(undefined).map((_, i) => i);
}