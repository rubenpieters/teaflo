import { Pool, mkButtonPool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { createPosition } from "../../util/position";
import { addText } from "../../phaser/datasprite";

export class LevelScreen {

  constructor(
    public readonly gameRefs: GameRefs
  ) {
  }

}
