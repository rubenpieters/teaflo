import { focus, over, set } from "src/shared/iassign-util";
import { Tree, extendTree, Location } from "../tree";
import { Ability } from "./ability";
import { GameState } from "./state";
import { applyAction, Action } from "./action";
import { nextAI } from "./ai";
import { Log, emptyLog } from "./log";

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
): { state: GameState, log: Log } {
  return _runSolution(solution, loc, state, emptyLog());
}

export function _runSolution(
  solution: Solution,
  loc: Location,
  state: GameState,
  log: Log,
): { state: GameState, log: Log } {
  if (loc.length === 0) {
    return { state, log };
  }
  // Action (Fr) Phase
  const frLog: Action[] = [];
  const frAction = solution.tree.nodes[loc[0]].v
  state = applyAction(frAction, state);
  frLog.push(frAction);

  // Action (En) Phase
  const enLog: Action[] = [];
  state.enUnits.forEach((enUnit, i) => {
    if (enUnit !== undefined) {
      const enAction = enUnit.ai[enUnit.currentAI].action;
      // apply action
      state = applyAction(enAction, state);
      // forward enUnit AI
      // NOTE: cast is safe here if there is no way that a unit has been removed due to an action (eg. deaths)
      // TODO: do this via position id
      state = focus(state, over(x => x.enUnits[i], x => nextAI(state, <any>x)));
      enLog.push(enAction);
    }
  });

  const currentLog = {
    frAction: frLog,
    enAction: enLog,
  }
  return _runSolution(solution, loc.slice(1), state, currentLog);
}