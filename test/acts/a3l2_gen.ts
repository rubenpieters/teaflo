import { mkGameState, showGamestate } from "../../src/shared/game/state";
import { trySolutions } from "../util";
import { pre } from "fast-check";

possibilities(
  ["u_i", "u_ii", "u_iii"],
  ["c_i", "c_ii", "c_iii"],
  ["d_i", "d_ii", "d_iii", "d_iv"]
).forEach(l => {
  console.log("-----------------------------------------------------");
  console.log(`PARAMS -- ${l}`)

  const initState = mkGameState(
    l as any,
    ["a3l2_en1"],
  );

  trySolutions(initState, 10);
  console.log("-----------------------------------------------------");
});

function possibilities<A>(...lists: A[][]): A[][] {
  return _possibilities(lists);
}

function _possibilities<A>(lists: A[][]): A[][] {
  if (lists.length === 0) {
    return [[]];
  } else {
    const result = lists[0].map(a => {
      const tail = lists.slice(1);
      const rec = _possibilities(tail);
      const result = rec.map(recList => {
        return [a].concat(recList);
      });
      return result;
    });
    return concat(result);
  }
}

function concat<A>(lists: A[][]): A[] {
  return lists.reduce((prev, current) => {
    return prev.concat(current);
  }, []);
}