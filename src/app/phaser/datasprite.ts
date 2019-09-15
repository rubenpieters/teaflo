import { Position } from "../util/position";
import { GameRefs } from "../states/game";
import { PhaserTextStyle } from "phaser-ce";

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
    // hovering status:
    // a sprite is being hovered when it has received a mouse over event
    // but no hover out event
    hovering: boolean,
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
  const style = {
    fill: txtColor,
    fontSize,
    boundsAlignH: "center",
    boundsAlignV: "middle",
  };
  addTextWithStyle(gameRefs, sprite, pos, btnString, style);
}

export function addTextWithStyle<Data>(
  gameRefs: GameRefs,
  sprite: DataSprite<Data>,
  pos: Position,
  btnString: string,
  style: PhaserTextStyle,
) {
  const bounds = {
    xMin: 0,
    yMin: 0,
    xMax: pos.xMax - pos.xMin,
    yMax: pos.yMax - pos.yMin,
  }
  addTextWithStyleAndBounds(gameRefs, sprite, bounds, btnString, style);
}

export function addTextWithStyleAndBounds<Data>(
  gameRefs: GameRefs,
  sprite: DataSprite<Data>,
  bounds: Position,
  btnString: string,
  style: PhaserTextStyle,
) {
  const btnText = gameRefs.game.add.text(
    0, 0, btnString, style,
  );
  btnText.setTextBounds(bounds.xMin, bounds.yMin, bounds.xMax, bounds.yMax);
  sprite.addChild(btnText);
  
  sprite.events.onKilled.add(() => {
    btnText.destroy();
  });
  sprite.events.onDestroy.add(() => {
    btnText.destroy();
  });
}

export function addShader<Data>(
  gameRefs: GameRefs,
  sprite: DataSprite<Data>,
  shader: string,
) {
  sprite.filters = [ gameRefs.filters[shader] ];

  sprite.events.onKilled.add(() => {
    clearShader(sprite);
  });
  sprite.events.onDestroy.add(() => {
    clearShader(sprite);
  });
}

export function clearShader<Data>(
  sprite: DataSprite<Data>,
) {
  sprite.filters = null as any;
}