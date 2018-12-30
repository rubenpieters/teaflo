import { focus, over, set } from "src/shared/iassign-util";
import { Solution } from "src/shared/game/solution";
import { Location } from "src/shared/tree";
import { newSolution } from "../states/game";

type LockStatus = "locked" | "unlocked";

export type SolInfo = {
  solution: Solution,
  supply: (string | undefined)[],
  deploy: (string | undefined)[],
  loc: Location
}

export type SaveFileV1 = {
  version: "V1",
  actUnlocked: { [key: number]: LockStatus | undefined },
  levelUnlocked: { [key: string]: LockStatus | undefined },
  levelSolutions: { [key: string]: SolInfo[], },
  activeSolutions: { [key: string]: number, },
  activeAct: number,
  activeLevel: string,
  activeScreen: "menu" | "game",
}

/* 
  Act
*/

export function changeAct(
  saveFile: SaveFileV1,
  act: number,
): SaveFileV1 {
  return focus(saveFile, set(x => x.activeAct, act));
}

export function activeAct(
  saveFile: SaveFileV1,
) {
  return saveFile.activeAct;
}

export function actAvailable(
  saveFile: SaveFileV1,
  act: number,
) {
  return saveFile.actUnlocked[act] === "unlocked";
}

/* 
  Level
*/

export function changeLevel(
  saveFile: SaveFileV1,
  levelId: string,
): SaveFileV1 {
  return focus(saveFile, set(x => x.activeLevel, levelId));
}

export function initializeLevel(
  saveFile: SaveFileV1,
  levelId: string,
): SaveFileV1 {
  saveFile = changeLevel(saveFile, levelId);
  // if the savefile has no solutions yet, then create one
  if (saveFile.levelSolutions[levelId] === undefined) {
    saveFile = focus(saveFile,
      set(x => x.levelSolutions[levelId], [newSolution(levelId)]),
      // make it the active solution
      set(x => x.activeSolutions[levelId], 0),
    );
  }
  return saveFile;
}

export function activeLevel(
  saveFile: SaveFileV1,
) {
  return saveFile.activeLevel;
}

export function levelAvailable(
  saveFile: SaveFileV1,
  levelId: string,
) {
  return saveFile.levelUnlocked[levelId] === "unlocked";
}

/* 
  Screen
*/

export function changeScreen(
  saveFile: SaveFileV1,
  screen: "menu" | "game",
) {
  return focus(saveFile, set(x => x.activeScreen, screen));
}

export function inMenu(
  saveFile: SaveFileV1,
) {
  return saveFile.activeScreen === "menu";
}

export function inGame(
  saveFile: SaveFileV1,
) {
  return saveFile.activeScreen === "game";
}

/* 
  Solution
*/

export function addSolution(
  saveFile: SaveFileV1,
  levelId: string = activeLevel(saveFile),
) {
  if (saveFile.levelSolutions[levelId] === undefined) {
    // A solution should have been added already, but if not we can still add one
    console.log(`WARNING (addSolution): no solutions for level ${levelId}`);
    
    return focus(saveFile, set(x => x.levelSolutions[levelId], [newSolution(levelId)]));
  } else {
    return focus(saveFile, over(x => x.levelSolutions[levelId], x => x.concat(newSolution(levelId))));
  }
}

export function addAndActivateSolution(
  saveFile: SaveFileV1,
  levelId: string = activeLevel(saveFile),
) {
  saveFile = addSolution(saveFile, levelId);
  return focus(saveFile,
    set(x => x.activeSolutions[levelId], saveFile.levelSolutions[levelId].length - 1),
  );
}

export function activeSolId(
  saveFile: SaveFileV1,
) {
  return saveFile.activeSolutions[activeLevel(saveFile)];
}

export function changeSolId(
  saveFile: SaveFileV1,
  solId: number,
  levelId: string = activeLevel(saveFile),
) {
  return focus(saveFile, set(x => x.activeSolutions[levelId], solId));
}

export function activeSolInfo(
  saveFile: SaveFileV1,
): SolInfo {
  return saveFile.levelSolutions[activeLevel(saveFile)][activeSolId(saveFile)];
}

export function changeSolInfo(
  saveFile: SaveFileV1,
  solution: Solution,
  loc: Location,
  levelId: string = activeLevel(saveFile),
  solId: number = activeSolId(saveFile),
) {
  return focus(saveFile,
    set(x => x.levelSolutions[levelId][solId].solution, solution),
    set(x => x.levelSolutions[levelId][solId].loc, loc),
  );
}

export function changeLoc(
  saveFile: SaveFileV1,
  loc: Location,
  levelId: string = activeLevel(saveFile),
  solId: number = activeSolId(saveFile),
) {
  return focus(saveFile,
    set(x => x.levelSolutions[levelId][solId].loc, loc),
  );
}

/*
  Deploy
*/

export function swapSpots(
  saveFile: SaveFileV1,
  from: { pos: number, type: "supply" | "deploy" },
  to: { pos: number, type: "supply" | "deploy" },
  levelId: string = activeLevel(saveFile),
  solId: number = activeSolId(saveFile),
) {
  const originalInToPos: string | undefined = saveFile.levelSolutions[levelId][solId][to.type][to.pos];
  const originalInFromPos: string | undefined = saveFile.levelSolutions[levelId][solId][from.type][from.pos];
  return focus(saveFile,
    set(x => x.levelSolutions[levelId][solId][from.type][from.pos], originalInToPos),
    set(x => x.levelSolutions[levelId][solId][to.type][to.pos], originalInFromPos),
  );
}