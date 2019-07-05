import { mkGameState, showGamestate } from "../../src/shared/game/state";
import { trySolutions } from "../util";

/*
// Sol 1
const initState = mkGameState(
  ["u_i", "c_iii", "d_iv"],
  ["a3l4_en1",],
);

console.log(showGamestate(initState));

trySolutions(initState, 10);
*/

/*
// Sol 2
const initState = mkGameState(
  ["u_ii", "c_ii", "d_i"],
  ["a3l4_en1"],
);

console.log(showGamestate(initState));

trySolutions(initState, 13);
*/

/*
// Sol 3
const initState = mkGameState(
  ["u_iii", "c_i", "d_iii"],
  ["a3l4_en1"],
);

console.log(showGamestate(initState));

trySolutions(initState, 13);
*/


possibilities(
  ["u_i", "u_ii", "u_iii"],
  ["c_i", "c_ii", "c_iii"],
//  ["d_i", "d_ii", "d_iii", "d_iv"]
  ["d_i", "d_iii", "d_iv"]
).forEach(l => {
  console.log("-----------------------------------------------------");
  console.log(`PARAMS -- ${l}`)

  const initState = mkGameState(
    l as any,
    ["a3l2_en1"],
  );

  const wins = trySolutions(initState, 10, false);
  console.log(`WINS: ${wins}`);
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