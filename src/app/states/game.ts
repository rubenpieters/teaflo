import { GameScreenData } from "../screens/gameScreen";
import { SaveFileV1 } from "../savefile/rep";
import { ActSelectData } from "../screens/actSelect";
import { applyScreenEvent, mkChangeAct } from "../util/screenEvents";
import { LevelSelectData } from "../screens/levelSelect";
import { Solution } from "src/shared/game/solution";
import { emptyTree, Location } from "src/shared/tree";
import { SpritePool } from "../util/pool";

export type GameRefs = {
  actSelectData: ActSelectData,
  levelSelectData: LevelSelectData,
  gameScreenData: GameScreenData,
  hoverViewPool: Phaser.Group,
  saveFile: SaveFileV1,
}
let gameRefs: GameRefs;

const newSaveFile: SaveFileV1 = {
  version: "V1",
  actUnlocked: {
    0: "unlocked",
    1: "unlocked",
  },
  levelUnlocked: {
    "a1_l1": "unlocked",
  },
  levelSolutions: {
    "a1_l1": [newSolution()],
  },
  activeSolutions: {
    "a1_l1": 0,
  },
  activeAct: 0,
  activeLevel: "a1_l1",
}

export default class Game extends Phaser.State {
  public init(): void {
    // NOTE: order of creating these groups determines their z-index
    const actSelectData: ActSelectData = {
      btnPool: new SpritePool(this.game),
    }
    const levelSelectData: LevelSelectData = {
      btnPool: new SpritePool(this.game),
      spriteGroup: new Phaser.Group(this.game),
      cardSlotPool: new Phaser.Group(this.game),
      cardPool: new Phaser.Group(this.game),
      solBtnPool: new Phaser.Group(this.game),
    }
    const gameScreenData: GameScreenData = {
      spriteGroup: new Phaser.Group(this.game),
      unitPool: new Phaser.Group(this.game),
      unitHpPool: new Phaser.Group(this.game),
      unitAbilityPool: new Phaser.Group(this.game),
      solTreePool: [],
    }
    gameRefs = {
      actSelectData,
      levelSelectData,
      gameScreenData,
      hoverViewPool: new Phaser.Group(this.game),
      // TODO: read file from somewhere
      saveFile: newSaveFile,
    }
  }

  public create(): void {
    this.stage.backgroundColor = 0xDCDCDC;

    applyScreenEvent(mkChangeAct(0), this.game, gameRefs);
  }
}

export function setSelectScreenVisible(
  visible: boolean,
) {
  gameRefs.actSelectData.btnPool.visible = visible;
  gameRefs.levelSelectData.btnPool.visible = visible;
  gameRefs.levelSelectData.spriteGroup.visible = visible;
  gameRefs.levelSelectData.cardPool.visible = visible;
  gameRefs.levelSelectData.cardSlotPool.visible = visible;
  gameRefs.levelSelectData.solBtnPool.visible = visible;
}

export function setGameScreenVisible(
  visible: boolean,
) {
  gameRefs.gameScreenData.spriteGroup.visible = visible;
  gameRefs.gameScreenData.unitPool.visible = visible;
  gameRefs.gameScreenData.unitHpPool.visible = visible;
  gameRefs.gameScreenData.unitAbilityPool.visible = visible;
}

export function newSolution(): {
  solution: Solution,
  cardIds: (string | undefined)[],
  loc: Location,
} {
  return {
    solution: {
      win: false,
      tree: emptyTree()
    },
    cardIds: [],
    loc: [],
  };
}