import { GameRefs } from "../states/game";
import { createPosition, Position, inPosition } from "../util/position";
import { config } from "../config";
import { applyScreenEvent, mkAddSolution, mkChangeSolution } from "../util/screenEvents";

const NEUTRAL = 0;
const DOWN = 1;
const OVER = 2;

export function drawSolutionSelect(
  game: Phaser.Game,
  gameRefs: GameRefs,
  levelId: string,
) {
  gameRefs.levelSelectData.solBtnPool.killAll();

  let i = 0;
  // create a button for each solution in the savefile
  for (const sol of gameRefs.saveFile.levelSolutions[levelId]) {
    const solBtnPos = createPosition(
      "right", 1250, config.levelButtonWidth,
      "top", 500 + 250 * i, config.levelButtonHeight,
    );
    createSolButton(game, gameRefs, solBtnPos, "btn_level", i, levelId);
    i += 1;
  }
  // max 5 solutions for one level
  if (i < 5) {
    // create the '+' button to add a solution
    const addSolBtnPos = createPosition(
      "right", 1250, config.levelButtonWidth,
      "top", 500 + 250 * i, config.levelButtonHeight,
    );
    createSolButton(game, gameRefs, addSolBtnPos, "btn_level", i, levelId);
  }
}

export function createSolButton(
  game: Phaser.Game,
  gameRefs: GameRefs,
  pos: Position,
  key: string,
  index: number,
  levelId: string,
): Phaser.Sprite {
  let frame: number;
  const txtColor: string = "#FF0000";
  if (gameRefs.saveFile.activeSolutions[levelId] === index) {
    frame = DOWN;
  } else {
    frame = NEUTRAL;
  }
  const btnSprite: Phaser.Sprite = gameRefs.levelSelectData.solBtnPool.getFirstExists(false, true, pos.xMin, pos.yMin, key, frame);
  
  btnSprite.data.selecting = false;
  btnSprite.data.index = index;
  btnSprite.data.levelId = levelId;

  if (btnSprite.data.init === undefined || btnSprite.data.init === false) {
    btnSprite.inputEnabled = true;
    btnSprite.events.onInputDown.add(() => {
      btnSprite.data.selecting = true;
      if (gameRefs.saveFile.activeSolutions[btnSprite.data.levelId] === index) {
        // noop
      } else {
        btnSprite.frame = DOWN;
      }
    });
    btnSprite.events.onInputUp.add(() => {
      if (
        inPosition(pos, game.input.activePointer.x, game.input.activePointer.y)
      ) {
        if (gameRefs.saveFile.activeSolutions[btnSprite.data.levelId] === index) {
          // noop
        } else if (gameRefs.saveFile.levelSolutions[btnSprite.data.levelId].length > btnSprite.data.index) {
          applyScreenEvent(mkChangeSolution(btnSprite.data.levelId, btnSprite.data.index), game, gameRefs);
        } else {
          applyScreenEvent(mkAddSolution(btnSprite.data.levelId), game, gameRefs);
        }
      }
    });
    btnSprite.events.onInputOver.add(() => {
      if (gameRefs.saveFile.activeSolutions[btnSprite.data.levelId] === index) {
        // noop
      } else {
        if (btnSprite.data.selecting) {
          btnSprite.frame = DOWN;
        } else {
          btnSprite.frame = OVER;
        }
      }
    });
    btnSprite.events.onInputOut.add(() => {
      if (gameRefs.saveFile.activeSolutions[btnSprite.data.levelId] === index) {
        // noop
      } else {
        btnSprite.frame = NEUTRAL;
      }
    });
  
    btnSprite.events.onKilled.add(() => {
      btnSprite.data.btnText.destroy();
    });
    btnSprite.events.onDestroy.add(() => {
      btnSprite.data.btnText.destroy();
    });

    btnSprite.data.init = true;
  }
  let btnString: string;
  if (gameRefs.saveFile.levelSolutions[btnSprite.data.levelId].length > btnSprite.data.index) {
    btnString = `sol ${index}`;
  } else {
    btnString = "+";
  }
  const btnText = game.add.text(
    0, 0, btnString, {
      fill: txtColor,
      fontSize: 100,
      boundsAlignH: "center",
      boundsAlignV: "middle",
    }
  );
  btnText.setTextBounds(0, 0, pos.xMax - pos.xMin, pos.yMax - pos.yMin);
  btnSprite.addChild(btnText);
  btnSprite.data.btnText = btnText;

  return btnSprite;
}