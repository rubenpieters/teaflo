import { GameRefs } from "../states/game";
import { createPosition, Position, inPosition } from "../util/position";
import { config } from "../config";
import { levelEnUnitMap } from "../gameData";
import { GSprite } from "src/shared/phaser-util";
import { Ability } from "src/shared/game/ability";
import { extendSolution, Solution, runSolution } from "src/shared/game/solution";
import { applyScreenEvent } from "../util/screenEvents";
import * as SE from "../util/screenEvents";
import { Location, Tree } from "src/shared/tree";
import { Game, Button } from "phaser-ce";
import { mkGameState, GameState } from "src/shared/game/state";
import { Action } from "../../shared/game/action";
import { createButtonInPool, addText } from "../util/btn";
import { TargetType } from "../../shared/game/entityId";
import { OVER, NEUTRAL } from "../util/button";
import { Unit } from "../../shared/game/unit";

export type StatsScreenData = {
  spriteGroup: Phaser.Group,
  statsLabel?: Phaser.Text,
  abilitiesLabel?: Phaser.Text,
  texts: Phaser.Text[],
}

export function drawSolutionInfo(
  game: Phaser.Game,
  gameRefs: GameRefs,
  levelId: string,
) {
  gameRefs.gameScreenData.logBtnPool.killAll();

  const solId = gameRefs.saveFile.activeSolutions[levelId];
  const sol = gameRefs.saveFile.levelSolutions[levelId][solId];

  // make initial state
  const frUnits = gameRefs.saveFile.levelSolutions[levelId][solId].cardIds;
  const enUnits = levelEnUnitMap[levelId];
  const initState = mkGameState(frUnits, enUnits);
  const solResult = runSolution(sol.solution, sol.loc, initState);
  const solState = solResult.state;
  const solLog = solResult.log;

  // draw solution tree
  mkTree(game, gameRefs, levelId);

  // draw action log
  solLog.forEach((entry, actionIndex) => {
    const actionBtnPos = createPosition(
      "left", 50, config.logButtonWidth,
      "top", 600 + config.logButtonHeight * actionIndex, config.logButtonHeight,
    );
    createActionLogButton(game, gameRefs, entry.action, actionBtnPos, "btn_log", actionIndex);
  });

  // draw stats screen headers
  if (gameRefs.gameScreenData.statsScreenData.statsLabel === undefined) {
    const statsLblPos = createPosition(
      "left", 1000, 720,
      "bot", 540, 100,
    );
    const statsLbl = game.add.text(
      statsLblPos.xMin, statsLblPos.yMin, "Stats", {
        fill: "#000000",
        fontSize: 70,
        boundsAlignH: "center",
        boundsAlignV: "middle",
      }
    );
    statsLbl.setTextBounds(0, 0, statsLblPos.xMax - statsLblPos.xMin, statsLblPos.yMax - statsLblPos.yMin);
  }
  if (gameRefs.gameScreenData.statsScreenData.abilitiesLabel === undefined) {
    const statsLblPos = createPosition(
      "left", 1720, 720,
      "bot", 540, 100,
    );
    const statsLbl = game.add.text(
      statsLblPos.xMin, statsLblPos.yMin, "Abilities", {
        fill: "#000000",
        fontSize: 70,
        boundsAlignH: "center",
        boundsAlignV: "middle",
      }
    );
    statsLbl.setTextBounds(0, 0, statsLblPos.xMax - statsLblPos.xMin, statsLblPos.yMax - statsLblPos.yMin);
  }
}

export function drawCardInfo(
  game: Phaser.Game,
  gameRefs: GameRefs,
  id: number,
  type: TargetType,
  state: GameState,
) {
  gameRefs.gameScreenData.statsScreenData.texts.forEach(x => x.destroy());

  let arr: (Unit | undefined)[];
  if (type === "friendly") {
    arr = state.frUnits;
  } else {
    arr = state.enUnits;
  }
  const unit = arr[id];
  if (unit !== undefined) {
    const hpPos = createPosition(
      "left", 1000, 720,
      "bot", 440, 100,
    );
    const hpLbl = game.add.text(
      hpPos.xMin, hpPos.yMin, `${unit.hp}/${unit.maxHp} HP`, {
        fill: "#000000",
        fontSize: 70,
        boundsAlignH: "center",
        boundsAlignV: "middle",
      }
    );
    hpLbl.setTextBounds(0, 0, hpPos.xMax - hpPos.xMin, hpPos.yMax - hpPos.yMin);
    gameRefs.gameScreenData.statsScreenData.texts.push(hpLbl);
  
    const chPos = createPosition(
      "left", 1000, 720,
      "bot", 340, 100,
    );
    const chLbl = game.add.text(
      chPos.xMin, chPos.yMin, `${unit.charges}/${unit.maxCharges} CH`, {
        fill: "#000000",
        fontSize: 70,
        boundsAlignH: "center",
        boundsAlignV: "middle",
      }
    );
    chLbl.setTextBounds(0, 0, chPos.xMax - chPos.xMin, chPos.yMax - chPos.yMin);
    gameRefs.gameScreenData.statsScreenData.texts.push(chLbl);
  }
}

export function clearCardInfo(
  gameRefs: GameRefs,
) {
  gameRefs.gameScreenData.statsScreenData.texts.forEach(x => x.destroy());
}

function locToPos(
  loc: Location,
): { x: number, y: number } {
  let x = 0;
  let y = 0;
  for (const number of loc) {
    x = x + 50;
    y = y + 50 * number;
  }
  return { x, y };
}

function drawTree<A>(
  game: Game,
  gameRefs: GameRefs,
  tree: Tree<A>,
  loc: Location,
  x: number,
  y: number,
  levelId: string,
): Phaser.Graphics[] {
  let sprites: Phaser.Graphics[] = [];
  let i = 0;
  const oldPos = locToPos(loc);
  for (const node of tree.nodes) {
    const newLoc = loc.concat(i);

    const pos = locToPos(newLoc);

    const line: Phaser.Graphics = game.add.graphics(x + oldPos.x + 20, y + oldPos.y + 20, gameRefs.gameScreenData.spriteGroup);
    line.beginFill(0x4477CC);
    line.lineStyle(5, 0x4477CC, 1);
    line.lineTo(pos.x - oldPos.x, pos.y - oldPos.y);
    line.endFill();

    const sprite: Phaser.Graphics = game.add.graphics(x + pos.x, y + pos.y, gameRefs.gameScreenData.spriteGroup);
    const solId = gameRefs.saveFile.activeSolutions[levelId];
    const currentLoc = gameRefs.saveFile.levelSolutions[levelId][solId].loc;
    if (newLoc.toString() === currentLoc.toString()) {
      sprite.beginFill(0xFF77CC);
    } else {
      sprite.beginFill(0x4477CC);
    }
    sprite.drawRect(0, 0, 40, 40);
    sprite.endFill();
    sprite.inputEnabled = true;
    sprite.events.onInputDown.add((obj: any, pointer: Phaser.Pointer) => {
      if (pointer.leftButton.isDown) {
        //changeLoc(board, newLoc)
        applyScreenEvent(new SE.ChangeTreeLoc(newLoc, levelId), game, gameRefs);
      } else if (pointer.rightButton.isDown) {
        //board.loc = loc;
        //board.solution = cutTree(board.solution, loc);
        //mkSolution(board);
      }
    });

    sprites.push(line);
    sprites.push(sprite);
    const result = drawTree(game, gameRefs, node.tree, newLoc, x, y, levelId);
    sprites = sprites.concat(result);

    i += 1;
  }
  return sprites
}

function mkTree(
  game: Game,
  gameRefs: GameRefs,
  levelId: string,
) {
  // clear old
  for (const sprite of gameRefs.gameScreenData.solTreePool) {
    sprite.destroy();
  }
  const solId = gameRefs.saveFile.activeSolutions[levelId];
  const currentLoc = gameRefs.saveFile.levelSolutions[levelId][solId].loc;

  const x = 220;
  const y = 100;
  const sprite: Phaser.Graphics = game.add.graphics(x, y, gameRefs.gameScreenData.spriteGroup);
  if (currentLoc.length === 0) {
    sprite.beginFill(0xFF77CC);
  } else {
    sprite.beginFill(0x4477CC);
  }
  sprite.drawRect(0, 0, 40, 40);
  sprite.endFill();
  sprite.inputEnabled = true;
  sprite.events.onInputDown.add((obj: any, pointer: Phaser.Pointer) => {
    if (pointer.leftButton.isDown) {
      //changeLoc(board, [])
      applyScreenEvent(new SE.ChangeTreeLoc([], levelId), game, gameRefs);
    } else if (pointer.rightButton.isDown) {
      //board.loc = [];
      //board.solution = cutTree(board.solution, []);
      //mkTree(board);
    }
  });

  // create new
  const currentSolution = gameRefs.saveFile.levelSolutions[levelId][solId].solution;
  const sprites: Phaser.Graphics[] = drawTree(game, gameRefs, currentSolution.tree, [], x, y, levelId);

  gameRefs.gameScreenData.solTreePool = [sprite].concat(sprites);
}

type ActionLogButton = GSprite<{
  init: boolean,
  selecting: boolean,
  action: Action,
  btnText: Phaser.Text,
}>;

export function createActionLogButton(
  game: Phaser.Game,
  gameRefs: GameRefs,
  action: Action,
  pos: Position,
  key: string,
  index: number,
): ActionLogButton {
  const frame: number = 0;
  const txtColor: string = "#FF0000";
  
  const btn = createButtonInPool(
    game,
    gameRefs.gameScreenData.logBtnPool,
    pos,
    { action },
    key,
    frame,
    // onInputDown
    () => {
      //
    },
    // onInputUp
    () => {
      //
    },
    // onInputOver
    () => {
      btn.frame = OVER;
      applyScreenEvent(new SE.ShowIntermediateSol(index), game, gameRefs);
    },
    // onInputOut
    () => {
      btn.frame = NEUTRAL;
      applyScreenEvent(new SE.ClearIntermediateSol(), game, gameRefs);
    },
  );

  const btnString = action.tag;
  return addText(game, btn, pos, btnString, txtColor);
}