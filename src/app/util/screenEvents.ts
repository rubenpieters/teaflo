import { SaveFileV1 } from "../savefile/rep";
import { GameRefs, setSelectScreenVisible, setGameScreenVisible } from "../states/game";
import { drawActSelect } from "../screens/actSelect";
import { drawLevelSelect } from "../screens/levelSelect";
import { levelMap } from "../gameData";
import { drawSolutionSelect } from "../screens/solutionSelect";
import { drawLevelInfo } from "../screens/levelInfo";
import { drawGameScreen } from "../screens/gameScreen";

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
}

export function mkStartLevel(
  levelId: string,
): StartLevel {
  return {
    tag: "StartLevel",
    levelId,
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

type ChangeSolution = {
  tag: "ChangeSolution",
  levelId: string,
  solId: number,
}

export function mkChangeSolution(
  levelId: string,
  solId: number,
): ChangeSolution {
  return {
    tag: "ChangeSolution",
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
}

export function mkGoToMenu(
): GoToMenu {
  return {
    tag: "GoToMenu",
  }
}

type ScreenEvent
  = ChangeAct
  | ChangeLevel
  | StartLevel
  | AddSolution
  | ChangeSolution
  | DeployCard
  | GoToMenu
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
      drawGameScreen(game, gameRefs, screenEvent.levelId);
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
    case "ChangeSolution": {
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
      setGameScreenVisible(false);
      setSelectScreenVisible(true);
      return;
    }
  }
}

function newSolution() {
  return {
    solution: { win: false, },
    cardIds: [],
  }
}