import { SpritePool } from "./pool";
import { Position, inPosition } from "./position";
import { GSprite } from "src/shared/phaser-util";
import { Game } from "phaser-ce";

export function createButtonInPool<A extends {}>(
  game: Game,
  pool: SpritePool<Button>,
  pos: Position,
  a: A,
  key: string,
  frame?: number,
  onInputDown?: () => void,
  onInputUp?: () => void,
  onInputOver?: () => void,
  onInputOut?: () => void,
): GSprite<ButtonValues & A> {
  let btnSprite = pool.getFirstExists(false, true, pos.xMin, pos.yMin, key, frame);
  btnSprite.data = {
    ...<any>a,
    ...{ selecting: false },
    // only copy init property from old data
    ...{ init: btnSprite.data.init },
  }

  // clear old onKilled/onDestroy events
  btnSprite.events.onKilled.removeAll();
  btnSprite.events.onDestroy.removeAll();

  // initialize if not initialized yuet
  btnSprite = initialize(game, btnSprite, pos,
    onInputDown === undefined ? () => { return; } : onInputDown,
    onInputUp === undefined ? () => { return; } : onInputUp,
    onInputOver === undefined ? () => { return; } : onInputOver,
    onInputOut === undefined ? () => { return; } : onInputOut,
  );

  return (<GSprite<ButtonValues & A>>btnSprite);
}

type ButtonValues = {
  init: boolean,
  selecting: boolean,
};

export type Button = GSprite<{
  init: boolean,
  selecting: boolean,
}>;

function initialize(
  game: Game,
  btnSprite: Button,
  pos: Position,
  onInputDown: () => void,
  onInputUp: () => void,
  onInputOver: () => void,
  onInputOut: () => void,
): Button {
  if (btnSprite.data.init === undefined || btnSprite.data.init === false) {

    btnSprite.inputEnabled = true;
    btnSprite.events.onInputDown.add(() => {
      btnSprite.data.selecting = true;
      onInputDown();
    });
    btnSprite.events.onInputUp.add(() => {
      if (
        inPosition(pos, game.input.activePointer.x, game.input.activePointer.y)
      ) {
        onInputUp();
      }
    });
    btnSprite.events.onInputOver.add(() => {
      onInputOver();
    });
    btnSprite.events.onInputOut.add(() => {
      onInputOut();
    });

    btnSprite.data.init = true;
  }

  return btnSprite;
}

export function addText<A>(
  game: Game,
  btnSprite: GSprite<A>,
  pos: Position,
  btnString: string,
  txtColor: string,
): GSprite<A & { btnText: Phaser.Text}> {
  const btnSpriteWTxt = (<GSprite<A & { btnText?: Phaser.Text}>>btnSprite);
  const btnText = game.add.text(
    0, 0, btnString, {
      fill: txtColor,
      fontSize: 100,
      boundsAlignH: "center",
      boundsAlignV: "middle",
    }
  );
  btnText.setTextBounds(0, 0, pos.xMax - pos.xMin, pos.yMax - pos.yMin);
  btnSpriteWTxt.addChild(btnText);
  btnSpriteWTxt.data.btnText = btnText;
  
  btnSprite.events.onKilled.add(() => {
    if (btnSpriteWTxt.data.btnText !== undefined) {
      btnSpriteWTxt.data.btnText.destroy();
      btnSpriteWTxt.data.btnText = undefined;
    }
  });
  btnSprite.events.onDestroy.add(() => {
    if (btnSpriteWTxt.data.btnText !== undefined) {
      btnSpriteWTxt.data.btnText.destroy();
      btnSpriteWTxt.data.btnText = undefined;
    }
  });

  return <GSprite<A & { btnText: Phaser.Text}>>btnSpriteWTxt;
}