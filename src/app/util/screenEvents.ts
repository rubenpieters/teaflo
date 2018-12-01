import { SaveFileV1 } from "../savefile/rep";
import { GameRefs, setSelectScreenVisible, setGameScreenVisible, newSolution } from "../states/game";
import { drawActSelect } from "../screens/actSelect";
import { drawLevelSelect } from "../screens/levelSelect";
import { levelMap } from "../gameData";
import { drawSolutionSelect } from "../screens/solutionSelect";
import { drawLevelInfo } from "../screens/levelInfo";
import { drawGameScreen } from "../screens/gameScreen";
import { applyUnlocks } from "../savefile/unlocks";
import { drawSolution } from "../screens/solutionRep";
import { Solution, extendSolution, SolutionData } from "src/shared/game/solution";
import { Ability } from "src/shared/game/ability";
import { Location } from "src/shared/tree";
import { ClickState } from "./clickState";

export type ChangeAct = {
  tag: "ChangeAct",
  actId: number,
}

export function mkChangeAct(
  actId: number,
): ChangeAct {
  return {
    tag: "ChangeAct",
    actId,
  }
}

type ChangeLevel = {
  tag: "ChangeLevel",
  levelId: string,
}

export function mkChangeLevel(
  levelId: string,
): ChangeLevel {
  return {
    tag: "ChangeLevel",
    levelId,
  }
}

type StartLevel = {
  tag: "StartLevel",
  levelId: string,
  solId: number,
}

export function mkStartLevel(
  levelId: string,
  solId: number,
): StartLevel {
  return {
    tag: "StartLevel",
    levelId,
    solId,
  }
}

type AddSolution = {
  tag: "AddSolution",
  levelId: string,
}

export function mkAddSolution(
  levelId: string,
): AddSolution {
  return {
    tag: "AddSolution",
    levelId,
  }
}

type ChangeActiveSolution = {
  tag: "ChangeActiveSolution",
  levelId: string,
  solId: number,
}

export function mkChangeActiveSolution(
  levelId: string,
  solId: number,
): ChangeActiveSolution {
  return {
    tag: "ChangeActiveSolution",
    levelId,
    solId,
  }
}

type DeployCard = {
  tag: "DeployCard",
  levelId: string,
  cardId: string,
  solId: number,
  from: { pos: number, type: "supply" | "deploy" },
  to: { pos: number, type: "supply" | "deploy" },
}

export function mkDeployCard(
  levelId: string,
  cardId: string,
  solId: number,
  from: { pos: number, type: "supply" | "deploy" },
  to: { pos: number, type: "supply" | "deploy" },
): DeployCard {
  return {
    tag: "DeployCard",
    levelId,
    cardId,
    solId,
    from,
    to,
  }
}

type GoToMenu = {
  tag: "GoToMenu",
  levelId: string,
}

export function mkGoToMenu(
  levelId: string,
): GoToMenu {
  return {
    tag: "GoToMenu",
    levelId,
  }
}

type ExtendLevelSolution = {
  tag: "ExtendLevelSolution",
  solData: SolutionData,
  levelId: string,
}

export function mkExtendLevelSolution(
  solData: SolutionData,
  levelId: string,
): ExtendLevelSolution {
  return {
    tag: "ExtendLevelSolution",
    solData,
    levelId,
  }
}

type ChangeTreeLoc = {
  tag: "ChangeTreeLoc",
  loc: Location,
  levelId: string,
}

export function mkChangeTreeLoc(
  loc: Location,
  levelId: string,
): ChangeTreeLoc {
  return {
    tag: "ChangeTreeLoc",
    loc,
    levelId,
  }
}

type CutTreeLoc = {
  tag: "CutTreeLoc",
  loc: Location,
  levelId: string,
}

export function mkCutTreeLoc(
  loc: Location,
  levelId: string,
): CutTreeLoc {
  return {
    tag: "CutTreeLoc",
    loc,
    levelId,
  }
}

type SetClickState = {
  tag: "SetClickState",
  clickState: ClickState,
}

export function mkSetClickState(
  ability: Ability,
): SetClickState {
  return {
    tag: "SetClickState",
    clickState: {
      ability,
      currentInputs: [],
    },
  }
}

type AdvanceClickState = {
  tag: "AdvanceClickState",
  input: any,
}

export function mkAdvanceClickState(
  input: any,
): AdvanceClickState {
  return {
    tag: "AdvanceClickState",
    input,
  }
}

type ScreenEvent
  = ChangeAct
  | ChangeLevel
  | StartLevel
  | AddSolution
  | ChangeActiveSolution
  | DeployCard
  | GoToMenu
  | ExtendLevelSolution
  | ChangeTreeLoc
  | CutTreeLoc
  | SetClickState
  | AdvanceClickState
  ;

export function applyScreenEvent(
  screenEvent: ScreenEvent,
  game: Phaser.Game,
  gameRefs: GameRefs,
): void {
  switch (screenEvent.tag) {
    case "ChangeAct": {
      gameRefs.saveFile.activeAct = screenEvent.actId;

      drawActSelect(game, gameRefs);
      const firstLevelId: string | undefined = levelMap[screenEvent.actId][0];
      if (firstLevelId === undefined) {
        console.log(`ERROR (applyScreenEvent ChangeAct): no levels for act ${screenEvent.actId}`);
        throw `applyScreenEvent ChangeAct: no levels for act ${screenEvent.actId}`;
      }
      applyScreenEvent(mkChangeLevel(firstLevelId), game, gameRefs);
      return;
    }
    case "ChangeLevel": {
      gameRefs.saveFile.activeLevel = screenEvent.levelId;
      // if the savefile has no solutions yet, then create one
      if (gameRefs.saveFile.levelSolutions[screenEvent.levelId] === undefined) {
        gameRefs.saveFile.levelSolutions[screenEvent.levelId] = [newSolution()];
        // make it the active solution
        gameRefs.saveFile.activeSolutions[screenEvent.levelId] = 0;
      }

      drawLevelSelect(game, gameRefs, gameRefs.saveFile.activeAct);
      drawLevelInfo(game, gameRefs, gameRefs.saveFile.activeLevel, gameRefs.saveFile.activeSolutions[screenEvent.levelId]);
      drawSolutionSelect(game, gameRefs, gameRefs.saveFile.activeLevel);
      return;
    }
    case "StartLevel": {
      gameRefs.gameScreenData.levelId = screenEvent.levelId;
      // TODO: temporary --
      gameRefs.saveFile.levelSolutions[screenEvent.levelId][screenEvent.solId].solution.win = true;
      // ------------------

      drawGameScreen(game, gameRefs, screenEvent.levelId);
      drawSolution(game, gameRefs, screenEvent.levelId);
      setSelectScreenVisible(false);
      setGameScreenVisible(true);
      return;
    }
    case "AddSolution": {
      if (gameRefs.saveFile.levelSolutions[screenEvent.levelId] === undefined) {
        // A solution should have been added already, but if not we can still add one
        console.log(`WARNING (applyScreenEvent AddSolution): no solutions for level ${screenEvent.levelId}`);
        gameRefs.saveFile.levelSolutions[screenEvent.levelId] = [newSolution()];
      } else {
        gameRefs.saveFile.levelSolutions[screenEvent.levelId].push(newSolution());
      }
      // change solution
      gameRefs.saveFile.activeSolutions[screenEvent.levelId] = gameRefs.saveFile.levelSolutions[screenEvent.levelId].length - 1;

      drawSolutionSelect(game, gameRefs, gameRefs.saveFile.activeLevel);
      drawLevelInfo(game, gameRefs, gameRefs.saveFile.activeLevel, gameRefs.saveFile.activeSolutions[screenEvent.levelId]);
      return;
    }
    case "ChangeActiveSolution": {
      gameRefs.saveFile.activeSolutions[screenEvent.levelId] = screenEvent.solId;

      drawSolutionSelect(game, gameRefs, gameRefs.saveFile.activeLevel);
      drawLevelInfo(game, gameRefs, gameRefs.saveFile.activeLevel, gameRefs.saveFile.activeSolutions[screenEvent.levelId]);
      return;
    }
    case "DeployCard": {
      if (screenEvent.from.type === "supply" && screenEvent.to.type === "supply") {
        // noop
      } else if (screenEvent.from.type === "deploy" && screenEvent.to.type === "deploy") {
        // swap
        const original: string | undefined = gameRefs.saveFile.levelSolutions[screenEvent.levelId][screenEvent.solId].cardIds[screenEvent.to.pos];
        gameRefs.saveFile.levelSolutions[screenEvent.levelId][screenEvent.solId].cardIds[screenEvent.from.pos] = original;
        gameRefs.saveFile.levelSolutions[screenEvent.levelId][screenEvent.solId].cardIds[screenEvent.to.pos] = screenEvent.cardId;
      } else if (screenEvent.from.type === "supply" && screenEvent.to.type === "deploy") {
        // put
        gameRefs.saveFile.levelSolutions[screenEvent.levelId][screenEvent.solId].cardIds[screenEvent.to.pos] = screenEvent.cardId;
      } else if (screenEvent.from.type === "deploy" && screenEvent.to.type === "supply") {
        // remove
        gameRefs.saveFile.levelSolutions[screenEvent.levelId][screenEvent.solId].cardIds[screenEvent.to.pos] = undefined;
      }

      // no need to redraw, this is handled by the sprites themselves
      return;
    }
    case "GoToMenu": {
      applyUnlocks(gameRefs);

      drawActSelect(game, gameRefs);
      drawLevelSelect(game, gameRefs, gameRefs.saveFile.activeAct);
      drawLevelInfo(game, gameRefs, gameRefs.saveFile.activeLevel, gameRefs.saveFile.activeSolutions[screenEvent.levelId]);
      drawSolutionSelect(game, gameRefs, gameRefs.saveFile.activeLevel);
      setGameScreenVisible(false);
      setSelectScreenVisible(true);
      return;
    }
    case "ExtendLevelSolution": {
      const solId = gameRefs.saveFile.activeSolutions[screenEvent.levelId];
      const currentSolution = gameRefs.saveFile.levelSolutions[screenEvent.levelId][solId].solution;
      const currentLoc = gameRefs.saveFile.levelSolutions[screenEvent.levelId][solId].loc;
      const newSolution = extendSolution(screenEvent.solData, currentSolution, currentLoc);
      gameRefs.saveFile.levelSolutions[screenEvent.levelId][solId].solution = newSolution.solution;
      gameRefs.saveFile.levelSolutions[screenEvent.levelId][solId].loc = newSolution.loc;
      // TODO: change state

      drawSolution(game, gameRefs, screenEvent.levelId);
      return;
    }
    case "ChangeTreeLoc": {
      const solId = gameRefs.saveFile.activeSolutions[screenEvent.levelId];
      gameRefs.saveFile.levelSolutions[screenEvent.levelId][solId].loc = screenEvent.loc;

      drawSolution(game, gameRefs, screenEvent.levelId);
      return;
    }
    case "CutTreeLoc": {
      return;
    }
    case "SetClickState": {
      gameRefs.gameScreenData.clickState = screenEvent.clickState;

      drawSolution(game, gameRefs, gameRefs.gameScreenData.levelId);
      return;
    }
    case "AdvanceClickState": {
      if (gameRefs.gameScreenData.clickState === undefined) {
        console.log(`ERROR (applyScreenEvent AdvanceClickState): cannot advance undefined clickState`);
        throw `applyScreenEvent AdvanceClickState: cannot advance undefined clickState`;
      }
      const clickState = gameRefs.gameScreenData.clickState;
      clickState.currentInputs.push(screenEvent.input);
      if (clickState.currentInputs.length >= clickState.ability.inputs.length) {
        // click state is finished, extend solution
        gameRefs.gameScreenData.clickState = undefined;
        applyScreenEvent(mkExtendLevelSolution({
            ability: clickState.ability,
            inputs: clickState.currentInputs,
          }, gameRefs.gameScreenData.levelId), game, gameRefs);
      } else {
        drawSolution(game, gameRefs, gameRefs.gameScreenData.levelId);
      }
      return;
    }
  }
}