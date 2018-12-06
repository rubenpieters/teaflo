import { focus, over, set } from "src/shared/iassign-util";
import { Tree, extendTree, Location } from "../tree";
import { Ability } from "./ability";
import { GameState } from "./state";
import { applyAction, Action } from "./action";
import { nextAI } from "./ai";
import { Log, emptyLog, LogEntry } from "./log";
import { intentToAction, Intent, Context } from "./intent";
import { Omit } from "../type-util";

export type SolutionData = {
  ability: Ability,
  inputs: any[],
}

export type Solution = {
  win: boolean,
  tree: Tree<SolutionData>,
}

export function extendSolution(
  solData: SolutionData,
  solution: Solution,
  loc: Location,
): {
  solution: Solution,
  loc: Location,
} {
  const newTree = extendTree((a1, a2) => false, solution.tree, loc, solData);
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
  console.log(solution);
  return _runSolution(solution.tree, loc, state, emptyLog());
}

export function _runSolution(
  tree: Tree<SolutionData>,
  loc: Location,
  state: GameState,
  log: Log,
): { state: GameState, log: Log } {
  if (loc.length === 0) {
    return { state, log };
  }
  // Action (Fr) Phase
  const solData: SolutionData = tree.nodes[loc[0]].v;
  const frAbility: Ability = solData.ability;
  const frInputs: any[] = solData.inputs;
  const frActionResult = applyIntentToSolution(frAbility.intent, { input: frInputs }, state);
  state = frActionResult.state;
  log = log.concat(frActionResult.log);

  // Action (En) Phase
  state.enUnits.forEach((enUnit, i) => {
    if (enUnit !== undefined) {
      const enAction = enUnit.ai[enUnit.currentAI].action;
      // apply action
      const enActionResult = applyActionsToSolution([enAction], { }, state, []);
      state = enActionResult.state;
      // forward enUnit AI
      // NOTE: cast is safe here if there is no way that a unit has been removed due to an action (eg. deaths)
      // TODO: do this via position id
      state = focus(state, over(x => x.enUnits[i], x => nextAI(state, <any>x)));
      log = log.concat(enActionResult.log);
    }
  });

  return _runSolution(tree.nodes[loc[0]].tree, loc.slice(1), state, log);
}

function applyIntentToSolution(
  intent: Intent,
  context: Omit<Context, "state">,
  state: GameState,
): {
  state: GameState,
  log: LogEntry[],
} {
  const action = intentToAction({ ...context, ...{ state: state } }, intent);
  return applyActionsToSolution([action], context, state, []);
}

function applyActionsToSolution(
  actions: Action[],
  context: Omit<Context, "state">,
  state: GameState,
  log: LogEntry[],
): {
  state: GameState,
  log: LogEntry[],
} {
  let newQueue: Action[] = [];
  const addLog: LogEntry[] = [];
  actions.forEach((action) => {
    const actionResult = applyAction(action, state);
    state = actionResult.state;
    newQueue = newQueue.concat(actionResult.actions);
    addLog.push({ action, state, })
  });
  if (newQueue.length === 0) {
    return { state, log: log.concat(addLog) };
  } else {
    return applyActionsToSolution(newQueue, context, state, log.concat(addLog));
  }
}