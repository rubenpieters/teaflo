import { Position } from "../util/position";
import { GameRefs } from "../states/game";

export interface DataSprite<Data> extends Phaser.Sprite {
  data: Data
  props?: {
    // init status:
    // a sprite is initialized once, which sets this flag to prevent reinitialization
    init: boolean,
    // selecting status:
    // a sprite is being selected when it has received a pointer down event
    // but the pointer has not been lifted up yet
    selecting: boolean,
  }
}

export function addText<Data>(
  gameRefs: GameRefs,
  sprite: DataSprite<Data>,
  pos: Position,
  btnString: string,
  txtColor: string,
  fontSize: number,
) {
  const btnText = gameRefs.game.add.text(
    0, 0, btnString, {
      fill: txtColor,
      fontSize,
      boundsAlignH: "center",
      boundsAlignV: "middle",
    }
  );
  btnText.setTextBounds(0, 0, pos.xMax - pos.xMin, pos.yMax - pos.yMin);
  sprite.addChild(btnText);
  
  sprite.events.onKilled.add(() => {
    btnText.destroy();
  });
  sprite.events.onDestroy.add(() => {
    btnText.destroy();
  });
}