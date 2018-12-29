import { SpritePool } from "./pool";
import { Position, inPosition } from "./position";
import { GSprite } from "src/shared/phaser-util";
import { Game } from "phaser-ce";

function wrapPopupOver(
  f: (() => void) | undefined,
  self: Phaser.Sprite,
) {
  return function() {
    if (self.data.popupF !== undefined) {
      self.data.popup = self.data.popupF(self);
    }
    if (f !== undefined) {
      f();
    }
  }
}

function wrapPopupOut(
  f: (() => void) | undefined,
  self: Phaser.Sprite,
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

type ButtonCallbacks<A> = {
  onDown?: () => void,
  clickLeft?: () => void,
  clickRight?: () => void,
  hoverOut?: () => void,
  hoverOver?: () => void,
  popupSprite?: (self: GSprite<ButtonValues & A>) => Phaser.Sprite,
};

export function createButtonInPool<A extends {}>(
  game: Game,
  pool: SpritePool<Button>,
  pos: Position,
  a: A,
  key: string,
  callbacks: ButtonCallbacks<A>,
  frame?: number,
): GSprite<ButtonValues & A> {
  let btnSprite = pool.getFirstExists(false, true, pos.xMin, pos.yMin, key, frame);
  btnSprite.data = {
    ...<any>a,
    ...{ selectingStatus: "none", popupSprite: callbacks.popupSprite },
    // only copy init property from old data
    ...{ init: btnSprite.data.init },
  }

  // clear old onKilled/onDestroy events
  btnSprite.events.onKilled.removeAll();
  btnSprite.events.onDestroy.removeAll();

  // initialize if not initialized yet
  btnSprite = initialize(game, btnSprite, pos,
    callbacks.onDown === undefined ? () => { return; } : callbacks.onDown,
    () => {
      if (btnSprite.data.selectingStatus === "right") {
        if (callbacks.clickRight !== undefined) {
          callbacks.clickRight();
        }
      } else if (btnSprite.data.selectingStatus === "left") {
        if (callbacks.clickLeft !== undefined) {
          callbacks.clickLeft();
        }
      }
    },
    wrapPopupOver(callbacks.hoverOver, btnSprite),
    wrapPopupOut(callbacks.hoverOut, btnSprite),
  );
  
  const result = (<GSprite<ButtonValues & A>>btnSprite);
  result.events.onKilled.add(() => {
    if (result.data.popup !== undefined) {
      result.data.popup.destroy();
      result.data.popup = undefined;
    }
  });
  result.events.onDestroy.add(() => {
    if (result.data.popup !== undefined) {
      result.data.popup.destroy();
      result.data.popup = undefined;
    }
  });

  return result;
}

export type ButtonValues = {
  init: boolean,
  selectingStatus: "left" | "right" | "none",
  popup?: Phaser.Sprite,
  popupSprite?: () => Phaser.Sprite,
};

export type Button = GSprite<ButtonValues>;

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
      btnSprite.data.selectingStatus = game.input.activePointer.rightButton.isDown ? "right" : "left";
      onInputDown();
    });
    btnSprite.events.onInputUp.add(() => {
      if (
        inPosition(pos, game.input.activePointer.x, game.input.activePointer.y)
      ) {
        onInputUp();
        btnSprite.data.selectingStatus = "none";
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
): GSprite<A & { btnText: Phaser.Text }> {
  const btnSpriteWTxt = (<GSprite<A & { btnText?: Phaser.Text }>>btnSprite);
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