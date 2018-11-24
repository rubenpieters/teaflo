import { focus, over, set } from "src/shared/iassign-util";
import { Tree, extendTree, Location } from "../tree";
import { Ability } from "./ability";
import { GameState } from "./state";
import { applyAction } from "./action";

export type Solution = {
  win: boolean,
  tree: Tree<Ability>,
}

export function extendSolution(
  ability: Ability,
  solution: Solution,
  loc: Location,
): {
  solution: Solution,
  loc: Location,
} {
  const newTree = extendTree((a1, a2) => false, solution.tree, loc, ability);
  return {
    solution: focus(solution, set(x => x.tree, newTree.tree)),
    loc: newTree.loc,
  };
}

export function runSolution(
  solution: Solution,
  loc: Location,
  state: GameState,
): GameState {
  if (loc.length === 0) {
    return state;
  }
  state = applyAction(solution.tree.nodes[loc[0]].v, state);
  return runSolution(solution, loc.slice(1), state);
}