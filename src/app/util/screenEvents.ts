import { focus, over, set } from "src/shared/iassign-util";
import { SaveFileV1, changeAct, changeLevel, changeScreen, initializeLevel, addSolution, addAndActivateSolution, activeLevel, activeSolId, changeSolId, swapDeployed, toDeploy, toSupply, activeAct, activeSolInfo, changeSolInfo, changeLoc } from "../savefile/rep";
import { GameRefs, setSelectScreenVisible, setGameScreenVisible, newSolution } from "../states/game";
import { drawActSelect } from "../screens/actSelect";
import { drawLevelSelect } from "../screens/levelSelect";
import { levelMap } from "../gameData";
import { drawSolutionSelect } from "../screens/solutionSelect";
import { drawLevelInfo } from "../screens/levelInfo";
import { drawGameScreen } from "../screens/gameScreen";
import { applyUnlocks } from "../savefile/unlocks";
import { drawSolutionRep } from "../screens/solutionRep";
import { Solution, extendSolution, SolutionData, cutSolution } from "src/shared/game/solution";
import { Ability } from "src/shared/game/ability";
import { Location, cutTree } from "src/shared/tree";
import { ClickState } from "./clickState";
import { drawSolutionInfo, drawCardInfo } from "../screens/solutionInfo";
import { TargetType } from "../../shared/game/entityId";
import { drawHoverCardFriendly, clearHoverCard } from "../screens/hoverCard";
import { Omit } from "src/shared/type-util";
import { LogKeys } from "src/shared/game/log";

/**
 * Changes the currently active act.
 * The active act is the act selected on the menu.
 */
export class ChangeAct {
  constructor(
    public readonly actId: number,
    public readonly tag: "ChangeAct" = "ChangeAct",
  ) {}
}

/**
 * Changes the currently active level.
 * The active level is the level selected on the menu.
 */
export class ChangeLevel {
  constructor(
    public readonly levelId: string,
    public readonly animation: boolean,
    public readonly tag: "ChangeLevel" = "ChangeLevel",
  ) {}
}

export class StartLevel {
  constructor(
    public readonly tag: "StartLevel" = "StartLevel",
  ) {}
}

export class AddSolution {
  constructor(
    public readonly tag: "AddSolution" = "AddSolution",
  ) {}
}

export class ChangeActiveSolution {
  constructor(
    public readonly solId: number,
    public readonly tag: "ChangeActiveSolution" = "ChangeActiveSolution",
  ) {}
}

export class DeployCard {
  constructor(
    public readonly cardId: string,
    public readonly solId: number,
    public readonly from: { pos: number, type: "supply" | "deploy" },
    public readonly to: { pos: number, type: "supply" | "deploy" },
    public readonly tag: "DeployCard" = "DeployCard",
  ) {}
}

export class GoToMenu {
  constructor(
    public readonly tag: "GoToMenu" = "GoToMenu",
  ) {}
}

export class ExtendLevelSolution {
  constructor(
    public readonly solData: SolutionData,
    public readonly tag: "ExtendLevelSolution" = "ExtendLevelSolution",
  ) {}
}

export class ChangeTreeLoc {
  constructor(
    public readonly loc: Location,
    public readonly tag: "ChangeTreeLoc" = "ChangeTreeLoc",
  ) {}
}

export class CutTreeLoc {
  constructor(
    public readonly loc: Location,
    public readonly tag: "CutTreeLoc" = "CutTreeLoc",
  ) {}
}

export class SetClickState {
  constructor(
    public readonly clickState: ClickState,
    public readonly tag: "SetClickState" = "SetClickState",
  ) {}
}

export class AdvanceClickState {
  constructor(
    public readonly input: any,
    public readonly tag: "AdvanceClickState" = "AdvanceClickState",
  ) {}
}

export class ShowIntermediateSol {
  constructor(
    public readonly index: number,
    public readonly type: LogKeys,
    public readonly tag: "ShowIntermediateSol" = "ShowIntermediateSol",
  ) {}
}

export class ClearIntermediateSol {
  constructor(
    public readonly tag: "ClearIntermediateSol" = "ClearIntermediateSol",
  ) {}
}

export class ShowHoverCard {
  constructor(
    public readonly type: TargetType,
    public readonly id: string,
    public readonly x: number,
    public readonly y: number,
    public readonly tag: "ShowHoverCard" = "ShowHoverCard",
  ) {}
}

export class ClearHoverCard {
  constructor(
    public readonly tag: "ClearHoverCard" = "ClearHoverCard",
  ) {}
}

export class ShowCardInfo {
  constructor(
    public readonly id: number,
    public readonly type: TargetType,
    public readonly tag: "ShowCardInfo" = "ShowCardInfo",
  ) {}
}

export class ClearCardInfo {
  constructor(
    public readonly tag: "ClearCardInfo" = "ClearCardInfo",
  ) {}
}

export class LockCardInfo {
  constructor(
    public readonly id: number,
    public readonly type: TargetType,
    public readonly tag: "LockCardInfo" = "LockCardInfo",
  ) {}
}

export class UnlockCardInfo {
  constructor(
    public readonly tag: "UnlockCardInfo" = "UnlockCardInfo",
  ) {}
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
  | ShowIntermediateSol
  | ClearIntermediateSol
  | ShowHoverCard
  | ClearHoverCard
  | ShowCardInfo
  | ClearCardInfo
  | LockCardInfo
  | UnlockCardInfo
  ;

export function applyScreenEvent(
  screenEvent: ScreenEvent,
  game: Phaser.Game,
  gameRefs: GameRefs,
): void {
  switch (screenEvent.tag) {
    case "ChangeAct": {
      gameRefs.saveFile = changeAct(gameRefs.saveFile, screenEvent.actId);

      drawActSelect(game, gameRefs);
      const firstLevelId: string | undefined = levelMap[screenEvent.actId][0];
      if (firstLevelId === undefined) {
        console.log(`ERROR (applyScreenEvent ChangeAct): no levels for act ${screenEvent.actId}`);
        throw `applyScreenEvent ChangeAct: no levels for act ${screenEvent.actId}`;
      }
      applyScreenEvent(new ChangeLevel(firstLevelId, true), game, gameRefs);
      return;
    }
    case "ChangeLevel": {
      gameRefs.saveFile = initializeLevel(gameRefs.saveFile, screenEvent.levelId);

      drawLevelSelect(game, gameRefs, activeAct(gameRefs.saveFile), screenEvent.animation);
      drawLevelInfo(game, gameRefs, activeLevel(gameRefs.saveFile), activeSolId(gameRefs.saveFile));
      drawSolutionSelect(game, gameRefs, activeLevel(gameRefs.saveFile));
      return;
    }
    case "StartLevel": {
      gameRefs.saveFile = changeScreen(gameRefs.saveFile, "game");

      drawGameScreen(game, gameRefs);
      drawSolutionRep(game, gameRefs, activeLevel(gameRefs.saveFile));
      drawSolutionInfo(game, gameRefs, activeLevel(gameRefs.saveFile));
      setSelectScreenVisible(false);
      setGameScreenVisible(true);
      return;
    }
    case "AddSolution": {
      gameRefs.saveFile = addAndActivateSolution(gameRefs.saveFile);

      drawSolutionSelect(game, gameRefs, activeLevel(gameRefs.saveFile));
      drawLevelInfo(game, gameRefs, activeLevel(gameRefs.saveFile), activeSolId(gameRefs.saveFile));
      return;
    }
    case "ChangeActiveSolution": {
      gameRefs.saveFile = changeSolId(gameRefs.saveFile, screenEvent.solId);

      drawSolutionSelect(game, gameRefs, activeLevel(gameRefs.saveFile));
      drawLevelInfo(game, gameRefs, activeLevel(gameRefs.saveFile), activeSolId(gameRefs.saveFile));
      return;
    }
    case "DeployCard": {
      if (screenEvent.from.type === "supply" && screenEvent.to.type === "supply") {
        // noop
      } else if (screenEvent.from.type === "deploy" && screenEvent.to.type === "deploy") {
        // swap
        gameRefs.saveFile = swapDeployed(gameRefs.saveFile, screenEvent.from.pos, screenEvent.to.pos, screenEvent.cardId);
      } else if (screenEvent.from.type === "supply" && screenEvent.to.type === "deploy") {
        // put
        gameRefs.saveFile = toDeploy(gameRefs.saveFile, screenEvent.to.pos, screenEvent.cardId);
      } else if (screenEvent.from.type === "deploy" && screenEvent.to.type === "supply") {
        // remove
        gameRefs.saveFile = toSupply(gameRefs.saveFile, screenEvent.to.pos);
      }

      // no need to redraw, this is handled by the sprites themselves
      return;
    }
    case "GoToMenu": {
      gameRefs.saveFile = changeScreen(gameRefs.saveFile, "menu");
      applyUnlocks(gameRefs);
      // reset hover/lock info
      gameRefs.gameScreenData.hoverInfo = undefined;
      gameRefs.gameScreenData.lockInfo = undefined;

      drawActSelect(game, gameRefs);
      drawLevelSelect(game, gameRefs, activeAct(gameRefs.saveFile), true);
      drawLevelInfo(game, gameRefs, activeLevel(gameRefs.saveFile), activeSolId(gameRefs.saveFile));
      drawSolutionSelect(game, gameRefs, activeLevel(gameRefs.saveFile));
      drawCardInfo(game, gameRefs, gameRefs.gameScreenData.state);
      setGameScreenVisible(false);
      setSelectScreenVisible(true);
      return;
    }
    case "ExtendLevelSolution": {
      const solInfo = activeSolInfo(gameRefs.saveFile);
      const currentSolution = solInfo.solution;
      const currentLoc = solInfo.loc;
      const newSolution = extendSolution(screenEvent.solData, currentSolution, currentLoc);
      gameRefs.saveFile = changeSolInfo(gameRefs.saveFile, newSolution.solution, newSolution.loc);

      drawSolutionRep(game, gameRefs, activeLevel(gameRefs.saveFile));
      drawSolutionInfo(game, gameRefs, activeLevel(gameRefs.saveFile));
      if (gameRefs.gameScreenData.lockInfo === undefined) {
        applyScreenEvent(new ClearCardInfo(), game, gameRefs);
      } else {
        drawCardInfo(game, gameRefs, gameRefs.gameScreenData.state);
      }
      return;
    }
    case "ChangeTreeLoc": {
      gameRefs.saveFile = changeLoc(gameRefs.saveFile, screenEvent.loc);

      drawSolutionRep(game, gameRefs, activeLevel(gameRefs.saveFile));
      drawSolutionInfo(game, gameRefs, activeLevel(gameRefs.saveFile));
      if (gameRefs.gameScreenData.lockInfo === undefined) {
        applyScreenEvent(new ClearCardInfo(), game, gameRefs);
      } else {
        drawCardInfo(game, gameRefs, gameRefs.gameScreenData.state);
      }
      return;
    }
    case "CutTreeLoc": {
      const solInfo = activeSolInfo(gameRefs.saveFile);
      const currentSolution = solInfo.solution;
      const newSolution = cutSolution(currentSolution, screenEvent.loc);
      gameRefs.saveFile = changeSolInfo(gameRefs.saveFile, newSolution.solution, newSolution.loc);

      drawSolutionRep(game, gameRefs, activeLevel(gameRefs.saveFile));
      drawSolutionInfo(game, gameRefs, activeLevel(gameRefs.saveFile));
      if (gameRefs.gameScreenData.lockInfo === undefined) {
        applyScreenEvent(new ClearCardInfo(), game, gameRefs);
      } else {
        drawCardInfo(game, gameRefs, gameRefs.gameScreenData.state);
      }
      return;
    }
    case "SetClickState": {
      gameRefs.gameScreenData.clickState = {...gameRefs.gameScreenData.clickState, ...screenEvent.clickState};

      drawSolutionRep(game, gameRefs, activeLevel(gameRefs.saveFile));
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
        applyScreenEvent(new ExtendLevelSolution({
            ability: clickState.ability,
            origin: clickState.origin,
            inputs: clickState.currentInputs,
          }), game, gameRefs);
      } else {
        drawSolutionRep(game, gameRefs, activeLevel(gameRefs.saveFile));
      }
      return;
    }
    case "ShowIntermediateSol": {
      drawSolutionRep(game, gameRefs, activeLevel(gameRefs.saveFile), {
        index: screenEvent.index,
        type: screenEvent.type,
      });
      if (gameRefs.gameScreenData.lockInfo === undefined) {
        applyScreenEvent(new ClearCardInfo(), game, gameRefs);
      } else {
        drawCardInfo(game, gameRefs, gameRefs.gameScreenData.state);
      }
      return;
    }
    case "ClearIntermediateSol": {
      drawSolutionRep(game, gameRefs, activeLevel(gameRefs.saveFile));
      if (gameRefs.gameScreenData.lockInfo === undefined) {
        applyScreenEvent(new ClearCardInfo(), game, gameRefs);
      } else {
        drawCardInfo(game, gameRefs, gameRefs.gameScreenData.state);
      }
      return;
    }
    case "ShowHoverCard": {
      switch (screenEvent.type) {
        case "friendly": {
          drawHoverCardFriendly(game, gameRefs, screenEvent.id, screenEvent.x, screenEvent.y);
          return;
        }
        case "enemy": {
          // drawHoverCard(game, gameRefs, screenEvent.id, screenEvent.x, screenEvent.y);
          return;
        }
      }
      return;
    }
    case "ClearHoverCard": {
      clearHoverCard(gameRefs);
      return;
    }
    case "ShowCardInfo": {
      gameRefs.gameScreenData.hoverInfo = {
        id: screenEvent.id,
        type: screenEvent.type
      };

      drawCardInfo(game, gameRefs, gameRefs.gameScreenData.state);
      return;
    }
    case "ClearCardInfo": {
      gameRefs.gameScreenData.hoverInfo = undefined;

      drawCardInfo(game, gameRefs, gameRefs.gameScreenData.state);
      return;
    }
    case "LockCardInfo": {
      gameRefs.gameScreenData.lockInfo = {
        id: screenEvent.id,
        type: screenEvent.type
      };

      drawCardInfo(game, gameRefs, gameRefs.gameScreenData.state);
      return;
    }
    case "UnlockCardInfo": {
      gameRefs.gameScreenData.lockInfo = undefined;

      drawCardInfo(game, gameRefs, gameRefs.gameScreenData.state);
      return;
    }
  }
}