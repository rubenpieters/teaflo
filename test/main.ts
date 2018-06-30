import { Board, generateBoard, boardData } from "src/shared/board";
import { newRng, rngHandler } from "src/shared/handler/rng/randomSeedRng";
import iassign from "immutable-assign";

//console.log(generateBoard(rngHandler(newRng("seed")), boardData));


/*
type Lens<S,T,A,B> = (s: S) => { f: (b: B) => T, a: A }

function scope<A>(a: A): Scope<A> {
  return new Scope(a);
}

class Scope<S> {
  readonly val: S;
  constructor(s: S) {
    this.val = s;
  }

  over<T,A,B>(g: (s: S) => A, s: (s: S, b: B) => T, m: (a: A) => B): Scope<T> {
    const a: A = g(this.val);
    const b: B = m(a);
    return scope(s(this.val, b));
  }

  over_<A>(g: (s: S) => A, m: (a: A) => A): Scope<S> {
    const a: A = g(this.val);
    const newS: S = iassign(this.val, g, m);
    return scope(newS);
  }


  /*set<T,A,B>(f: (b: B) => T, b: B): Scope<T> {
    return scope(f(b));
  }*/

  get<T,A,B>(g: (s: S) => A): A {
    return g(this.val);
  }
}

const x = { a: { b: 1, c: "2" }, d: 4 };

const n = scope(x).over(s => s.a.b, (s, b) => iassign(s, s => s.a.b, a => b), x => x + 1).val;

*/