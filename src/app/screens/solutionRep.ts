import { GameRefs } from "../states/game";
import { createPosition, relativeTo, Position, inPosition } from "../util/position";
import { config } from "../config";
import { levelEnUnitMap } from "../gameData";
import { GSprite } from "src/shared/phaser-util";
import { Ability } from "src/shared/game/ability";
import { extendSolution, Solution, runSolution } from "src/shared/game/solution";
import { applyScreenEvent, mkExtendLevelSolution, mkChangeTreeLoc } from "../util/screenEvents";
import { Location, Tree } from "src/shared/tree";
import { Game } from "phaser-ce";
import { mkGameState } from "src/shared/game/state";
import { Action } from "../../shared/game/action";
import { createButtonInPool, addText } from "../util/btn";

export function drawSolution(
  game: Phaser.Game,
  gameRefs: GameRefs,
  levelId: string,
) {
  gameRefs.gameScreenData.unitPool.killAll();
  gameRefs.gameScreenData.unitHpPool.killAll();
  gameRefs.gameScreenData.unitAbilityPool.killAll();

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
  let logIndex = 0;
  solLog.frAction.forEach((action, actionIndex) => {
    const actionBtnPos = createPosition(
      "left", 250, config.levelButtonWidth,
      "bot", 1200 - config.levelButtonHeight * logIndex, config.levelButtonHeight,
    );
    createActionLogButton(game, gameRefs, action, actionBtnPos, "btn_level");
    logIndex += 1;
  });
  solLog.enAction.forEach((action, actionIndex) => {
    const actionBtnPos = createPosition(
      "left", 250, config.levelButtonWidth,
      "bot", 1200 - config.levelButtonHeight * logIndex, config.levelButtonHeight,
    );
    createActionLogButton(game, gameRefs, action, actionBtnPos, "btn_level");
    logIndex += 1;
  });

  // draw friendly units
  solState.frUnits.forEach((unit, unitIndex) => {
    if (unit !== undefined) {

      const unitPos = createPosition(
        "left", 750 + 200 * unitIndex, config.levelSelectCardWidth,
        "bot", 750, config.levelSelectCardHeight,
      );
      createUnit(game, gameRefs, unitPos, unit.cardId);

      const unitHpPos = relativeTo(unitPos,
        "below", 50,
        config.unitHpBarWidth, config.unitHpBarHeight,
      );
      unitHpPos.xMax = unitHpPos.xMin + (unitHpPos.xMax - unitHpPos.xMin) * (unit.hp / unit.maxHp);
      createUnitHp(game, gameRefs, unitHpPos, "hp");

      unit.abilities.forEach((ability, abilityIndex) => {
        const abilityPos = relativeTo(unitHpPos,
          "below", 50 + 110 * abilityIndex,
          config.unitHpBarWidth, config.unitHpBarHeight,
        );
        createUnitAbility(game, gameRefs, abilityPos, "ability", levelId, solId, ability);
      });
    }
  });

  // draw enemy units
  solState.enUnits.forEach((unit, unitIndex) => {
    if (unit !== undefined) {

      const unitPos = createPosition(
        "left", 2200, config.levelSelectCardWidth,
        "bot", 750, config.levelSelectCardHeight,
      );
      createUnit(game, gameRefs, unitPos, unit.cardId);

      const unitHpPos = relativeTo(unitPos,
        "below", 50,
        config.unitHpBarWidth, config.unitHpBarHeight,
      );
      unitHpPos.xMax = unitHpPos.xMin + (unitHpPos.xMax - unitHpPos.xMin) * (unit.hp / unit.maxHp);
      createUnitHp(game, gameRefs, unitHpPos, "hp");
    }
  });
}

type UnitSprite = GSprite<{
  init: boolean,
}>;

export function createUnit(
  game: Phaser.Game,
  gameRefs: GameRefs,
  pos: Position,
  key: string,
): UnitSprite {
  const unit: UnitSprite = gameRefs.gameScreenData.unitPool.getFirstExists(false, true, pos.xMin, pos.yMin, key);
  
  if (unit.data.init === undefined || unit.data.init === false) {

    unit.data.init = true;
  }

  return unit;
}

export function createUnitHp(
  game: Phaser.Game,
  gameRefs: GameRefs,
  pos: Position,
  key: string,
): UnitSprite {
  const unit: UnitSprite = gameRefs.gameScreenData.unitHpPool.getFirstExists(false, true, pos.xMin, pos.yMin, key);

  unit.width = pos.xMax - pos.xMin;
  
  if (unit.data.init === undefined || unit.data.init === false) {

    unit.data.init = true;
  }

  return unit;
}

type AbilitySprite = GSprite<{
  init: boolean,
  levelId: string,
  solId: number,
  ability: Ability,
}>;

export function createUnitAbility(
  game: Phaser.Game,
  gameRefs: GameRefs,
  pos: Position,
  key: string,
  levelId: string,
  solId: number,
  ability: Ability,
): AbilitySprite {
  const unit: AbilitySprite = gameRefs.gameScreenData.unitAbilityPool.getFirstExists(false, true, pos.xMin, pos.yMin, key);
  
  unit.data.levelId = levelId;
  unit.data.solId = solId;
  unit.data.ability = ability;

  if (unit.data.init === undefined || unit.data.init === false) {
    unit.inputEnabled = true;
    unit.events.onInputUp.add(() => {
      if (
        inPosition(pos, game.input.activePointer.x, game.input.activePointer.y)
      ) {
        applyScreenEvent(mkExtendLevelSolution(unit.data.ability, unit.data.levelId), game, gameRefs);
      }
    });

    unit.data.init = true;
  }

  return unit;
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

function drawTree(
  game: Game,
  gameRefs: GameRefs,
  tree: Tree<Ability>,
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
        applyScreenEvent(mkChangeTreeLoc(newLoc, levelId), game, gameRefs);
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
      applyScreenEvent(mkChangeTreeLoc([], levelId), game, gameRefs);
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
      //
    },
    // onInputOut
    () => {
      //
    },
  );

  const btnString = action.tag;
  return addText(game, btn, pos, btnString, txtColor);
}