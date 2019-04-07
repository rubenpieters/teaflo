import { Ability, resolveAbility } from "./ability";
import { UnitId } from "./entityId";
import { Tree, emptyTree, extendTree, Location, cutTree } from "../tree";
import { focus, set } from "../iassign-util";
import { GameState } from "./state";
import { Log, emptyLog, LogEntry } from "./log";
import { StartTurn, ignoreTag, Action, resolveAction } from "./action";
import { Context, StartTurnContext, FrAbilityContext, EnAbilityContext } from "./context";
import { applyStatuses } from "./status";
import { aiPosToIndex } from "./ai";
import { EnAbility } from "./unit";

export type SolutionData = {
  ability: Ability,
  origin: UnitId,
  inputs: any[],
}

export type Solution = {
  win: boolean,
  tree: Tree<SolutionData>,
}

export const emptySolution: Solution = {
  win: false,
  tree: emptyTree(),
};

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

/**
 * Run Solution
 */
export function runSolution(
  solution: Solution,
  loc: Location,
  state: GameState,
): { state: GameState, log: Log, win: boolean } {
  if (loc.length === 0) {
    const result = runInitialTurn(state);
    return { state: result.state, log: result.log, win: false };
  } else {
    return _runSolution(solution.tree, loc, state, emptyLog(), false);
  }
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

export function endStates(
  tree: Tree<SolutionData>,
  state: GameState,
): GameState[] {
  if (tree.nodes.length === 0) {
    return [state];
  } else {
    let result: GameState[] = [];
    for (const node of tree.nodes) {
      const solData = node.v;
  
      const phasesResult = runPhases(state, solData);
      const rec = endStates(node.tree, phasesResult.state);
      result = result.concat(rec);
    }
    return result;
  }
}

/**
 * Run Phases
 */

function runInitialTurn(
  state: GameState,
) {
  let log: Log = emptyLog();

  // The initial turn applies a `StartTurn` action on the game
  const context = new StartTurnContext();
  const startTurnResult = applyActionsToSolution([new StartTurn], context, context, state, [], 0);
  state = startTurnResult.state;
  const stFilteredLog = startTurnResult.log.filter(x => ! ignoreTag(x.action.tag));
  log = log.concat(stFilteredLog);

  if (state.type === "invalid") {
    return { state, log: log, win: false };
  }
  return { state, log };
}

export function runPhases(
  state: GameState,
  solData: SolutionData,
) {
  let log: Log = emptyLog();

  // Start Turn Phase
  const startTurnResult = runInitialTurn(state);
  state = startTurnResult.state;
  log = log.concat(startTurnResult.log);

  if (state.type === "invalid") {
    return { state, log: log, win: false };
  }

  // Action (Fr) Phase
  const frAbility: Ability = solData.ability;
  const frInputs: any[] = solData.inputs;
  const frContext = new FrAbilityContext(solData.origin, frInputs);
  const frActionResult = applyAbilityToSolution(frAbility, frContext, state, 1);
  state = frActionResult.state;
  const frFilteredLog = frActionResult.log.filter(x => ! ignoreTag(x.action.tag));
  log = log.concat(frFilteredLog);

  if (state.type === "invalid") {
    return { state, log: log, win: false };
  }

  // Action (En) Phase
  let i = 0;
  state.enIds().forEach(t => {
    const enId = t.e;
    const result = state.overTarget(enId, x => x);
    const entity = result.entity;
    // the entity can be undefined because an action in this loop caused a change in the state
    if (entity !== undefined) {
      const enAbility: Ability = entity.abilities[aiPosToIndex(entity.aiPosition)].ability;
      const enContext = new EnAbilityContext(enId);
      const enActionResult = applyAbilityToSolution(enAbility, enContext, state, i + 2);
      state = enActionResult.state;
      const enFilteredLog = enActionResult.log.filter(x => ! ignoreTag(x.action.tag));
      log = log.concat(enFilteredLog);

      i += 1;
    }
  });

  const win = checkWin(state);
  if (win) {
    state = focus(state, set(x => x.type, "win"));
  }
  return { state, log, win };
}

export function checkWin(
  state: GameState,
): boolean {
  // Check Win
  const enHps = state.enFiltered()
    .map(x => x.e.hp)
    ;
  const countAllBelow0 = enHps
    .filter(x => x <= 0)
    .length
    ;

  return countAllBelow0 === state.enFiltered().length;
}

function applyAbilityToSolution(
  ability: Ability,
  context: Context,
  state: GameState,
  typeIndex: number,
): {
  state: GameState,
  log: LogEntry[],
} {
  const actions = resolveAbility(ability, state, context);
  return applyActionsToSolution(actions, context, context, state, [], typeIndex);
}

function applyActionsToSolution(
  actions: Action[],
  originalContext: Context,
  context: Context,
  state: GameState,
  log: LogEntry[],
  typeIndex: number,
  entryIndex = 0,
  actionIndex = 0,
): {
  state: GameState,
  log: LogEntry[],
} {
  let newQueue: Action[] = [];
  const addLog: LogEntry[] = [];
  for (const action of actions) {
    const { actions, transformed, transforms } = applyStatuses(action, state, context);
    const actionResult = resolveAction(state, transformed);
    state = actionResult.state;
    newQueue = newQueue.concat(actionResult.actions).concat(actions);
    addLog.push({ action: transformed, state, transforms, context, typeIndex, entryIndex, actionIndex });

    if (state.type === "invalid") {
      return { state, log: log.concat(addLog) };
    }
    if (! ignoreTag(action.tag)) {
      entryIndex = entryIndex + 1;
    }
  }
  if (newQueue.length === 0) {
    return { state, log: log.concat(addLog) };
  } else {
    // TODO: here the original context stays unchanged, but the context should change throughout these calls
    // for example, the self property should change
    return applyActionsToSolution(newQueue, context, context, state, log.concat(addLog), typeIndex, entryIndex, actionIndex + 1);
  }
}