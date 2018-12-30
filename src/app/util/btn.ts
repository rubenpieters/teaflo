import { SpritePool } from "./pool";
import { Position, inPosition } from "./position";
import { GSprite } from "src/shared/phaser-util";
import { Game } from "phaser-ce";

function wrapPopupOver(
  f: (() => void) | undefined,
  self: Phaser.Sprite,
) {
  return function() {
    if (self.data.popupSprite !== undefined) {
      self.data.popup = self.data.popupSprite(self);
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
  dragStart?: () => void,
  dragStop?: () => void,
  dragUpdate?: () => void,
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
  // give new type to btnSprite, since its data field has changed
  let newBtnSprite = <GSprite<ButtonValues & A>>btnSprite;

  // clear old onKilled/onDestroy events
  newBtnSprite.events.onKilled.removeAll();
  newBtnSprite.events.onDestroy.removeAll();

  // initialize if not initialized yet
  newBtnSprite = initialize(game, newBtnSprite, pos,
    callbacks.onDown === undefined ? () => { return; } : callbacks.onDown,
    () => {
      if (newBtnSprite.data.selectingStatus === "right") {
        if (callbacks.clickRight !== undefined) {
          callbacks.clickRight();
        }
      } else if (newBtnSprite.data.selectingStatus === "left") {
        if (callbacks.clickLeft !== undefined) {
          callbacks.clickLeft();
        }
      }
    },
    wrapPopupOver(callbacks.hoverOver, newBtnSprite),
    wrapPopupOut(callbacks.hoverOut, newBtnSprite),
    callbacks.dragStart,
    callbacks.dragStop,
    callbacks.dragUpdate,
  );
  
  // remove popup sprite when self is destroyed/killed
  newBtnSprite.events.onKilled.add(() => {
    if (newBtnSprite.data.popup !== undefined) {
      newBtnSprite.data.popup.destroy();
      newBtnSprite.data.popup = undefined;
    }
  });
  newBtnSprite.events.onDestroy.add(() => {
    if (newBtnSprite.data.popup !== undefined) {
      newBtnSprite.data.popup.destroy();
      newBtnSprite.data.popup = undefined;
    }
  });

  return newBtnSprite;
}

export type ButtonValues = {
  init: boolean,
  selectingStatus: "left" | "right" | "none",
  popup?: Phaser.Sprite,
  popupSprite?: () => Phaser.Sprite,
};

export type Button = GSprite<ButtonValues>;

function initialize<A extends {}>(
  game: Game,
  btnSprite: GSprite<ButtonValues & A>,
  pos: Position,
  onInputDown: () => void,
  onInputUp: () => void,
  onInputOver: () => void,
  onInputOut: () => void,
  onDragStart?: () => void,
  onDragStop?: () => void,
  onDragUpdate?: () => void,
): GSprite<ButtonValues & A> {
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
    btnSprite.events.onInputOver.add(onInputOver);
    btnSprite.events.onInputOut.add(onInputOut);

    if (onDragStart !== undefined || onDragStop !== undefined || onDragUpdate !== undefined) {
      btnSprite.input.enableDrag(false, true);
      if (onDragStart !== undefined) {
        btnSprite.events.onDragStart.add(onDragStart);
      }
      if (onDragStop !== undefined) {
        btnSprite.events.onDragStop.add(onDragStop);
      }
      if (onDragUpdate !== undefined) {
        btnSprite.events.onDragUpdate.add(onDragUpdate);
      }
    }

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