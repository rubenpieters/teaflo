import { LevelSelect, GameRefs } from "../states/game";
import { createPosition } from "../util/position";
import { createPoolButton } from "../util/poolButton";
import { config } from "../config";
import { createButton } from "../util/button";
import { levelSelect_LevelSlots } from "./general";

export function drawSolutionSelect(
  game: Phaser.Game,
  gameRefs: GameRefs,
  levelSelect: LevelSelect,
  levelId: string,
) {
  levelSelect.solBtnPool.forEachAlive((x: Phaser.Sprite) => x.kill());

  let i = 0;
  // the savefile has no solutions yet, then create one
  if (gameRefs.saveFile.levelSolutions[levelId] === undefined) {
    gameRefs.saveFile.levelSolutions[levelId] = [{
      solution: { win: false, },
      cardIds: [],
    }];
  }
  // create a button for each solution in the savefile
  let active: Phaser.Sprite | undefined = undefined;
  for (const sol of gameRefs.saveFile.levelSolutions[levelId]) {
    const solBtnPos = createPosition(
      "right", 1250, config.levelButtonWidth,
      "top", 500 + 250 * i, config.levelButtonHeight,
    );
    const solBtn = createPoolButton(game, levelSelect.solBtnPool, solBtnPos, `sol ${i}`, "btn_level",
      () => console.log("choose sol")
    );
    if (i === gameRefs.saveFile.activeSolutions[levelId]) {
      active = solBtn;
    }
    i += 1;
  }
  if (active !== undefined) {
    active.events.onInputUp.dispatch({ force: true });
  }
  // create the '+' button to add a solution
  if (levelSelect.addSolBtn !== undefined) {
    levelSelect.addSolBtn.destroy();
    levelSelect.addSolBtn = undefined;
  }
  if (gameRefs.saveFile.levelSolutions[levelId].length < 5) {
    const addSolBtnPos = createPosition(
      "right", 1250, config.levelButtonWidth,
      "top", 500 + 250 * i, config.levelButtonHeight,
    );
    const addSolBtn = createButton(game, levelSelect.group, addSolBtnPos, "+", "btn_level",
      () => {
        gameRefs.saveFile.levelSolutions[levelId].push({
          solution: { win: false, },
          cardIds: [],
        });
        levelSelect_LevelSlots(game, gameRefs, levelSelect, levelId);
      }
    );
    levelSelect.addSolBtn = addSolBtn;
  }
}