export function filterUndefined<A>(
  l: (A | undefined)[],
): A[] {
  return <A[]>l.filter(x => x !== undefined);
}