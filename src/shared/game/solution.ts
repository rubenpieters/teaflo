import { resolveAbility } from "./ability";
import { UnitId } from "../definitions/entityId";
import { Tree, emptyTree, extendTree, Location, cutTree } from "../tree";
import { focus, set } from "../iassign-util";
import { enIds, overTarget, enFiltered } from "./state";
import { Log, emptyLog, LogEntry, ActionSource, AbilitySource, RuleSource } from "./log";
import { ignoreTag, resolveAction } from "./action";
import { Context, FrAbilityContext, EnAbilityContext } from "../definitions/context";
import { applyStatuses } from "./status";
import { aiPosToIndex } from "./ai";
import { Ability } from "../definitions/ability";
import { GameState } from "../definitions/state";
import { StartTurn } from "../definitions/actionf";
import { ActionWithOrigin, Action } from "../definitions/action";
import { showEntityId } from "./entityId";

export type SolutionData = {
  ability: Ability,
  origin: UnitId,
  inputs: any[],
}

export function showSolData(
  solData: SolutionData,
): string {
  const posString = solData.origin === undefined ? "N/A" : showEntityId(solData.origin);
  return ` - ${posString} - ${JSON.stringify(solData.inputs)}`;
}
//${solData.ability.name}

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
): { state: GameState, prevState: GameState | undefined, log: Log, win: boolean } {
  if (loc.length === 0) {
    const result = runInitialTurn(state);
    return { state: result.state, prevState: undefined, log: result.log, win: false };
  } else {
    return _runSolution(solution.tree, loc, state, emptyLog(), false, undefined);
  }
}

export function _runSolution(
  tree: Tree<SolutionData>,
  loc: Location,
  state: GameState,
  log: Log,
  win: boolean,
  prevState: GameState | undefined = undefined,
): { state: GameState, prevState: GameState | undefined, log: Log, win: boolean } {
  if (loc.length === 0) {
    return { state, prevState, log, win };
  }
  const solData: SolutionData = tree.nodes[loc[0]].v;

  const phasesResult = runPhases(state, solData);

  return _runSolution(tree.nodes[loc[0]].tree, loc.slice(1), phasesResult.state, phasesResult.log, phasesResult.win, state);
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
  const startTurnResult = applyActionsToSolution([{...new StartTurn, origin: "noOrigin", actionSource: new RuleSource(0) }], state, [], 0, 0);
  state = startTurnResult.state;
  const stFilteredLog = startTurnResult.log.filter(x => ! ignoreTag(x.action.tag));
  log = log.concat(stFilteredLog);

  if (state.type === "invalid") {
    return { state, log: log, win: false, intermediateIndex: startTurnResult.intermediateIndex, };
  }
  return { state, log, intermediateIndex: startTurnResult.intermediateIndex, };
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
  const frActionResult = applyAbilityToSolution(frAbility, frContext, state, 1, startTurnResult.intermediateIndex);
  state = frActionResult.state;
  const frFilteredLog = frActionResult.log.filter(x => ! ignoreTag(x.action.tag));
  log = log.concat(frFilteredLog);

  if (state.type === "invalid") {
    return { state, log: log, win: false };
  }

  // Action (En) Phase
  let i = 0;
  let prevIntermediateIndex = frActionResult.intermediateIndex;
  enIds(state).forEach(t => {
    const enId = t.e;
    const result = overTarget(state, enId, x => x);
    const entity = result.entity;
    if (
      // the entity can be undefined because an action in this loop caused a change in the state
      entity !== undefined &&
      // an enemy only acts when its hp is higher than 0
      entity.hp > 0
    ) {
      const enAbility: Ability = entity.abilities[aiPosToIndex(entity.aiPosition)].ability;
      const enContext = new EnAbilityContext(enId);
      const enActionResult = applyAbilityToSolution(enAbility, enContext, state, i + 2, prevIntermediateIndex);
      state = enActionResult.state;
      const enFilteredLog = enActionResult.log.filter(x => ! ignoreTag(x.action.tag));
      log = log.concat(enFilteredLog);

      i += 1;
      prevIntermediateIndex = enActionResult.intermediateIndex;
    }
  });

  /*const win = checkWin(state);
  if (win) {
    state = focus(state, set(x => x.type, "win"));
  }*/
  return { state, log, win: state.type === "win" };
}

export function checkWin(
  state: GameState,
): boolean {
  // Check Win
  const enHps = enFiltered(state)
    .map(x => x.e.hp)
    ;
  const countAllBelow0 = enHps
    .filter(x => x <= 0)
    .length
    ;

  return countAllBelow0 === enFiltered(state).length;
}

function applyAbilityToSolution(
  ability: Ability,
  context: Context,
  state: GameState,
  typeIndex: number,
  intermediateIndex: number,
): {
  state: GameState,
  log: LogEntry[],
  intermediateIndex: number,
} {
  // resolve an ability into actions
  const actions: (Action & { actionSource: ActionSource })[] = resolveAbility(ability, state, context);
  // attach an origin to the actions, if applicable
  let actionsWithOrigins: (ActionWithOrigin & { actionSource: ActionSource })[] = actions.map((x, i) => { 
    return {...x, origin: context.self }
  });
  // apply each of the actions to the state
  return applyActionsToSolution(actionsWithOrigins, state, [], typeIndex, intermediateIndex);
}

function applyActionsToSolution(
  actions: (ActionWithOrigin & { actionSource: ActionSource })[],
  state: GameState,
  log: LogEntry[],
  typeIndex: number,
  intermediateIndex: number,
  entryIndex = 0,
  actionIndex = 0,
): {
  state: GameState,
  log: LogEntry[],
  intermediateIndex: number,
} {
  let newQueue: (ActionWithOrigin & { actionSource: ActionSource })[] = [];
  const addLog: LogEntry[] = [];
  for (const action of actions) {
    const { actions, transformed, transforms } = applyStatuses(action, state);
    const actionResult = resolveAction(state, transformed);
    state = actionResult.state;
    newQueue = newQueue.concat(actionResult.actions).concat(actions);
    addLog.push({ action: transformed, state, transforms, typeIndex, entryIndex, actionIndex, actionSource: action.actionSource, intermediateIndex });

    if (state.type === "invalid") {
      return { state, log: log.concat(addLog), intermediateIndex, };
    }
    if (! ignoreTag(action.tag)) {
      entryIndex = entryIndex + 1;
      intermediateIndex = intermediateIndex + 1;
    }
  }
  if (newQueue.length === 0) {
    return { state, log: log.concat(addLog), intermediateIndex, };
  } else {
    return applyActionsToSolution(newQueue, state, log.concat(addLog), typeIndex, intermediateIndex, entryIndex, actionIndex + 1);
  }
}