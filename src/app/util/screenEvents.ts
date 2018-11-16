import { SaveFileV1 } from "../savefile/rep";
import { GameRefs } from "../states/game";
import { drawActSelect } from "../screens/actSelect";
import { drawLevelSelect } from "../screens/levelSelect";

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

type ScreenEvent
  = ChangeAct
  | ChangeLevel
  | StartLevel
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
      // also change level
      break;
    }
    case "ChangeLevel": {
      gameRefs.saveFile.activeLevel = screenEvent.levelId;
      drawLevelSelect(game, gameRefs, gameRefs.saveFile.activeAct);
      // also change level info
      // also change solution slots
      break;  
    }
    case "StartLevel": {
      break;  
    }
  }
}