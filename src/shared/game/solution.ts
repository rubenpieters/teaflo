import { focus, over, set } from "src/shared/iassign-util";
import { Tree, extendTree, Location, cutTree, emptyTree } from "../tree";
import { Ability } from "./ability";
import { GameState, filteredEn } from "./state";
import { applyAction, Action } from "./action";
import { nextAI } from "./ai";
import { Log, emptyLog, LogEntry } from "./log";
import { intentToAction, Intent, Context } from "./intent";
import { Omit } from "../type-util";
import { UnitId, overEnemy, GlobalId } from "./entityId";
import { applyTriggers } from "./trigger";

export type SolutionData = {
  ability: Ability,
  origin: UnitId | undefined;
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

export function cutSolution(
  solution: Solution,
  loc: Location,
): {
  solution: Solution,
  loc: Location,
} {
  if (loc.length === 0) {
    return {
      solution: {
        tree: emptyTree(),
        win: false,
      },
      loc: [],
    };
  } else {
    const newTree = cutTree(solution.tree, loc);
    return {
      solution: focus(solution, set(x => x.tree, newTree)),
      loc: loc.slice(0, -1),
    };
  }
}

export function runSolution(
  solution: Solution,
  loc: Location,
  state: GameState,
): { state: GameState, log: Log, win: boolean } {
  // console.log(solution);
  return _runSolution(solution.tree, loc, state, emptyLog(), false);
}

export function _runSolution(
  tree: Tree<SolutionData>,
  loc: Location,
  state: GameState,
  log: Log,
  win: boolean,
): { state: GameState, log: Log, win: boolean } {
  if (loc.length === 0) {
    return { state, log, win };
  }
  const solData: SolutionData = tree.nodes[loc[0]].v;

  const phasesResult = runPhases(state, solData);

  return _runSolution(tree.nodes[loc[0]].tree, loc.slice(1), phasesResult.state, phasesResult.log, phasesResult.win);
}

export function findWin(
  tree: Tree<SolutionData>,
  state: GameState,
): boolean {
  for (const node of tree.nodes) {
    const solData = node.v;

    const phasesResult = runPhases(state, solData);
    const rec = findWin(node.tree, phasesResult.state);
    if (rec) {
      return true;
    }
  }
  return false;
}

function runPhases(
  state: GameState,
  solData: SolutionData,
) {

  let log: LogEntry[] = [];
  // Action (Fr) Phase
  const frAbility: Ability = solData.ability;
  const frInputs: any[] = solData.inputs;
  const frActionResult = applyIntentToSolution(frAbility.intent, { input: frInputs, self: solData.origin }, state);
  state = frActionResult.state;
  log = log.concat(frActionResult.log);

  if (state.state === "invalid") {
    return { state, log: log, win: false };
  }

  // Action (En) Phase
  state.enUnits.forEach((enUnit, i) => {
    if (enUnit !== undefined) {
      const enIntent = enUnit.ai[enUnit.currentAI].intent;
      // apply action
      const enUnitSelf: UnitId = new GlobalId(enUnit.id, "enemy");
      const enActionResult = applyIntentToSolution(enIntent, { self: enUnitSelf }, state);
      state = enActionResult.state;
      // forward enUnit AI
      state = overEnemy(enUnitSelf, state,
        x => nextAI(state, x),
        x => x,
      );
      log = log.concat(enActionResult.log);
    }
  });

  // Check Win
  const enHps = filteredEn(state)
    .map(x => x.hp)
    ;
  const countAllBelow0 = enHps
    .filter(x => x <= 0)
    .length
    ;
  
  const win = countAllBelow0 === filteredEn(state).length;
  return { state, log: log, win: win };
}

function applyIntentToSolution(
  intent: Intent,
  context: Context,
  state: GameState,
): {
  state: GameState,
  log: LogEntry[],
} {
  const action = intentToAction(state, context, intent);
  return applyActionsToSolution([action], context, state, []);
}

function applyActionsToSolution(
  actions: Action[],
  context: Context,
  state: GameState,
  log: LogEntry[],
): {
  state: GameState,
  log: LogEntry[],
} {
  let newQueue: Action[] = [];
  const addLog: LogEntry[] = [];
  for (const action of actions) {
    const { actions, transformed } = applyTriggers(state, action, context);
    const actionResult = applyAction(transformed, state);
    state = actionResult.state;
    newQueue = newQueue.concat(actionResult.actions).concat(actions);
    addLog.push({ action, state, });

    if (state.state === "invalid") {
      return { state, log: log.concat(addLog) };
    }
  }
  if (newQueue.length === 0) {
    return { state, log: log.concat(addLog) };
  } else {
    return applyActionsToSolution(newQueue, context, state, log.concat(addLog));
  }
}