import { SpritePool } from "./pool";
import { Position, inPosition } from "./position";
import { GSprite } from "src/shared/phaser-util";
import { Game } from "phaser-ce";

export type PopupInfo = {
  f: (game: Phaser.Game) => Phaser.Sprite,
}

function wrapPopupOver(
  f: (() => void) | undefined,
  game: Phaser.Game,
  self: Phaser.Sprite,
  popupInfo?: PopupInfo,
) {
  return function() {
    if (popupInfo !== undefined) {
      self.data.popup = popupInfo.f(game);
    }
    if (f !== undefined) {
      f();
    }
  }
}

function wrapPopupOut(
  f: (() => void) | undefined,
  game: Phaser.Game,
  self: Phaser.Sprite,
  popupInfo?: PopupInfo,
) {
  return function() {
    if (self.data.popup !== undefined) {
      self.data.popup.destroy();
      self.data.popup = undefined;
    }
    if (f !== undefined) {
      f();
    }
  }
}

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
  popupInfo?: PopupInfo,
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

  // initialize if not initialized yet
  btnSprite = initialize(game, btnSprite, pos,
    onInputDown === undefined ? () => { return; } : onInputDown,
    onInputUp === undefined ? () => { return; } : onInputUp,
    wrapPopupOver(onInputOver, game, btnSprite, popupInfo),
    wrapPopupOut(onInputOut, game, btnSprite, popupInfo),
  );

  return (<GSprite<ButtonValues & A>>btnSprite);
}

type ButtonValues = {
  init: boolean,
  selecting: boolean,
  popup?: Phaser.Sprite,
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
  fontSize: number,
): GSprite<A & { btnText: Phaser.Text}> {
  const btnSpriteWTxt = (<GSprite<A & { btnText?: Phaser.Text}>>btnSprite);
  const btnText = game.add.text(
    0, 0, btnString, {
      fill: txtColor,
      fontSize,
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