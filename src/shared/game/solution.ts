import { focus, over, set } from "../iassign-util";
import { Tree, extendTree, Location, cutTree, emptyTree } from "../tree";
import { Ability } from "./ability";
import { GameState, filteredEn, filteredFr } from "./state";
import { applyAction, Action, StartTurn, NextAI, ignoreTag } from "./action";
import { nextAI } from "./ai";
import { Log, emptyLog, LogEntry } from "./log";
import { intentToAction, Intent, Context } from "./intent";
import { Omit } from "../type-util";
import { UnitId, overEnemy, GlobalId, posToString } from "./entityId";
import { applyTriggers } from "./trigger";

export type SolutionData = {
  ability: Ability,
  origin: UnitId | undefined;
  inputs: any[],
}

export function showSolDataCompact(
  solData: SolutionData,
): string {
  const posString = solData.origin === undefined ? "N/A" : posToString(solData.origin);
  return `${solData.ability.name} - ${posString} - ${JSON.stringify(solData.inputs)}`;
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

// ------------------------------
// Run Solution

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

// ------------------------------
// Run Phases

function runInitialTurn(
  state: GameState,
) {
  let log: Log = emptyLog();

  const startTurnResult = applyActionsToSolution([new StartTurn], {}, {}, state, [], 0);
  state = startTurnResult.state;
  const stFilteredLog = startTurnResult.log.filter(x => ! ignoreTag(x.action.tag));
  log = log.concat(stFilteredLog);

  if (state.state === "invalid") {
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

  if (state.state === "invalid") {
    return { state, log: log, win: false };
  }

  // Action (Fr) Phase
  const frAbility: Ability = solData.ability;
  const frInputs: any[] = solData.inputs;
  const frActionResult = applyIntentToSolution(frAbility.intent, { input: frInputs, self: solData.origin }, state, 1);
  state = frActionResult.state;
  const frFilteredLog = frActionResult.log.filter(x => ! ignoreTag(x.action.tag));
  log = log.concat(frFilteredLog);

  if (state.state === "invalid") {
    return { state, log: log, win: false };
  }

  // Action (En) Phase
  state.enUnits.forEach((enUnit, i) => {
    if (enUnit !== undefined) {
      const enIntent = enUnit.ai[enUnit.currentAI].intent;
      // apply action
      const enUnitSelf: GlobalId<"enemy"> = new GlobalId(enUnit.id, "enemy");
      const enActionResult = applyIntentToSolution(enIntent, { self: enUnitSelf }, state, i + 2);
      state = enActionResult.state;
      // forward enUnit AI
      const context = { self: enUnitSelf };
      const enForwardAIResult = applyActionsToSolution(
        [new NextAI(enUnitSelf)], context, context, state, [], i + 2, enActionResult.log.length, Infinity, // TODO: what is a suitable action index?
      );
      state = enForwardAIResult.state;
      const enFilteredLog =
        enActionResult.log
          .concat(enForwardAIResult.log)
          .filter(x => ! ignoreTag(x.action.tag));
      log = log.concat(enFilteredLog);
    }
  });

  const win = checkWin(state);
  if (win) {
    state = focus(state, set(x => x.state, "win"));
  }
  return { state, log, win };
}

export function checkWin(
  state: GameState,
): boolean {
  // Check Win
  const enHps = filteredEn(state)
    .map(x => x.hp)
    ;
  const countAllBelow0 = enHps
    .filter(x => x <= 0)
    .length
    ;

  return countAllBelow0 === filteredEn(state).length;
}

function applyIntentToSolution(
  intent: Intent,
  context: Context,
  state: GameState,
  typeIndex: number,
): {
  state: GameState,
  log: LogEntry[],
} {
  const action = intentToAction(state, context, intent);
  return applyActionsToSolution([action], context, context, state, [], typeIndex);
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
    const { actions, transformed, transforms } = applyTriggers(state, action, context);
    const actionResult = applyAction(transformed, state);
    state = actionResult.state;
    newQueue = newQueue.concat(actionResult.actions).concat(actions);
    addLog.push({ action: transformed, state, transforms, originalContext, context, typeIndex, entryIndex, actionIndex });

    if (state.state === "invalid") {
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