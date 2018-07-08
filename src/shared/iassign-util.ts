import iassign from "immutable-assign";

type GetModOps<S, A> = { get: (s: S) => A, modify: (a: A) => A };
type GetMod<S> = <R>(e: <A>(gs: GetModOps<S, A>) => R) => R;

export function focus<S>(
  s: S,
  ...es: GetMod<S>[]
): S {
  return es.reduce((acc, e) => e(gs => iassign(acc, gs.get, gs.modify)), s);
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
  return e => e({ get, modify: x => a });
}

// example usage

const x = { a: 1, b: "2" };

focus(x,
  over(x => x.a, x => x + 1),
  over(x => x.b, x => x + "x"),
);
