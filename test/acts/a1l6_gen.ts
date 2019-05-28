import { mkGameState, showGamestate } from "../../src/shared/game/state";
import { trySolutions } from "../util";
import { pre } from "fast-check";

/*
const initState = mkGameState(
  ["a1l6_fr1", "a1l6_fr3", "a1l6_fr2"],
  ["a1l6_en1"],
);

console.log(showGamestate(initState));

trySolutions(initState, 10);
*/

permutations(["a1l6_fr1", "a1l6_fr3", "a1l6_fr2"]).forEach(l => {
  console.log("-----------------------------------------------------");
  console.log(`PARAMS -- ${l}`)

  const initState = mkGameState(
    l as any,
    ["a1l6_en1"],
  );

  trySolutions(initState, 10);
  console.log("-----------------------------------------------------");
});

function permutations<A>(
  as: A[],
): A[][] {
  if (as.length === 0) {
    return [];
  }
  if (as.length === 1) {
    return [as];
  }
  const result = as.map((a, i) => {
    const prev = as.slice(0, i).concat(as.slice(i + 1, as.length))
    const prevs = permutations(prev);
    return prevs.map(x => x.concat(a));
  });
  return result.reduce((prev, acc) => prev.concat(acc), []);
}