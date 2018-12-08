import { SaveFileV1 } from "../savefile/rep";
import { GameRefs, setSelectScreenVisible, setGameScreenVisible, newSolution } from "../states/game";
import { drawActSelect } from "../screens/actSelect";
import { drawLevelSelect } from "../screens/levelSelect";
import { levelMap } from "../gameData";
import { drawSolutionSelect } from "../screens/solutionSelect";
import { drawLevelInfo } from "../screens/levelInfo";
import { drawGameScreen } from "../screens/gameScreen";
import { applyUnlocks } from "../savefile/unlocks";
import { drawSolutionRep } from "../screens/solutionRep";
import { Solution, extendSolution, SolutionData } from "src/shared/game/solution";
import { Ability } from "src/shared/game/ability";
import { Location } from "src/shared/tree";
import { ClickState } from "./clickState";
import { drawSolutionInfo, drawCardInfo, clearCardInfo } from "../screens/solutionInfo";
import { TargetType } from "../../shared/game/entityId";
import { drawHoverCardFriendly, clearHoverCard } from "../screens/hoverCard";

export class ChangeAct {
  constructor(
    public readonly actId: number,
    public readonly tag: "ChangeAct" = "ChangeAct",
  ) {}
}

export class ChangeLevel {
  constructor(
    public readonly levelId: string,
    public readonly tag: "ChangeLevel" = "ChangeLevel",
  ) {}
}

export class StartLevel {
  constructor(
    public readonly levelId: string,
    public readonly solId: number,
    public readonly tag: "StartLevel" = "StartLevel",
  ) {}
}

export class AddSolution {
  constructor(
    public readonly levelId: string,
    public readonly tag: "AddSolution" = "AddSolution",
  ) {}
}

export class ChangeActiveSolution {
  constructor(
    public readonly levelId: string,
    public readonly solId: number,
    public readonly tag: "ChangeActiveSolution" = "ChangeActiveSolution",
  ) {}
}

export class DeployCard {
  constructor(
    public readonly levelId: string,
    public readonly cardId: string,
    public readonly solId: number,
    public readonly from: { pos: number, type: "supply" | "deploy" },
    public readonly to: { pos: number, type: "supply" | "deploy" },
    public readonly tag: "DeployCard" = "DeployCard",
  ) {}
}

export class GoToMenu {
  constructor(
    public readonly levelId: string,
    public readonly tag: "GoToMenu" = "GoToMenu",
  ) {}
}

export class ExtendLevelSolution {
  constructor(
    public readonly solData: SolutionData,
    public readonly levelId: string,
    public readonly tag: "ExtendLevelSolution" = "ExtendLevelSolution",
  ) {}
}

export class ChangeTreeLoc {
  constructor(
    public readonly loc: Location,
    public readonly levelId: string,
    public readonly tag: "ChangeTreeLoc" = "ChangeTreeLoc",
  ) {}
}

export class CutTreeLoc {
  constructor(
    public readonly loc: Location,
    public readonly levelId: string,
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
      applyScreenEvent(new ChangeLevel(firstLevelId), game, gameRefs);
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

      drawGameScreen(game, gameRefs, screenEvent.levelId);
      drawSolutionRep(game, gameRefs, screenEvent.levelId);
      drawSolutionInfo(game, gameRefs, screenEvent.levelId);
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

      drawSolutionRep(game, gameRefs, screenEvent.levelId);
      drawSolutionInfo(game, gameRefs, screenEvent.levelId);
      clearCardInfo(gameRefs);
      return;
    }
    case "ChangeTreeLoc": {
      const solId = gameRefs.saveFile.activeSolutions[screenEvent.levelId];
      gameRefs.saveFile.levelSolutions[screenEvent.levelId][solId].loc = screenEvent.loc;

      drawSolutionRep(game, gameRefs, screenEvent.levelId);
      drawSolutionInfo(game, gameRefs, screenEvent.levelId);
      clearCardInfo(gameRefs);
      return;
    }
    case "CutTreeLoc": {
      return;
    }
    case "SetClickState": {
      gameRefs.gameScreenData.clickState = screenEvent.clickState;

      drawSolutionRep(game, gameRefs, gameRefs.gameScreenData.levelId);
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
            inputs: clickState.currentInputs,
          }, gameRefs.gameScreenData.levelId), game, gameRefs);
      } else {
        drawSolutionRep(game, gameRefs, gameRefs.gameScreenData.levelId);
      }
      return;
    }
    case "ShowIntermediateSol": {
      drawSolutionRep(game, gameRefs, gameRefs.gameScreenData.levelId, {
        index: screenEvent.index,
      });
      clearCardInfo(gameRefs);
      return;
    }
    case "ClearIntermediateSol": {
      drawSolutionRep(game, gameRefs, gameRefs.gameScreenData.levelId);
      clearCardInfo(gameRefs);
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
      drawCardInfo(game, gameRefs, screenEvent.id, screenEvent.type, gameRefs.gameScreenData.state);

      return;
    }
    case "ClearCardInfo": {
      clearCardInfo(gameRefs);

      return;
    }
  }
}