import iassign from "immutable-assign";

type GetModOps<S, A> = { get: (s: S) => A, modify: (a: A) => A };
type GetMod<S> = <R>(e: <A>(gs: GetModOps<S, A>) => R) => R;

export function focus<S>(
  s: S,
  ...es: GetMod<S>[]
): S {
  return es.reduce((acc, e) => e(gs => iassign(acc, gs.get, gs.modify)), s);
}

export function modifyAndGet<S, A, B>(
  s: S,
  get: (s: S) => A,
  modify: (a: A) => { a: A, b: B },
): { s: S, b: B } {
  let b: B = undefined as any;
  const newS = iassign(s, get, x => {
    const result = modify(x);
    b = result.b;
    return result.a;
  });
  return { s: newS, b };
}

export function over<S, A>(
  get: (s: S) => A,
  modify: (a: A) => A,
): GetMod<S> {
  return e => e({ get, modify });
}

export function set<S, A>(
  get: (s: S) => A,
  a: A,
): GetMod<S> {
  return e => e({ get, modify: _ => a });
}
