import { SaveFileV1 } from "../savefile/rep";
import { GameRefs } from "../states/game";
import { drawActSelect } from "../screens/actSelect";
import { drawLevelSelect } from "../screens/levelSelect";
import { levelMap } from "../gameData";

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
      const firstLevelId: string | undefined = levelMap[screenEvent.actId][0]
      if (firstLevelId === undefined) {
        console.log(`ERROR (applyScreenEvent): no levels for act ${screenEvent.actId}`);
        throw `applyScreenEvent: no levels for act ${screenEvent.actId}`;
      }
      applyScreenEvent(mkChangeLevel(firstLevelId), game, gameRefs);
      break;
    }
    case "ChangeLevel": {
      console.log(`CHANGE LEVEL ${screenEvent.levelId}`);
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