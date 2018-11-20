import { GameRefs } from "../states/game";
import { unitMap } from "src/shared/data/units/units";
import { createPosition, relativeTo, Position } from "../util/position";
import { config } from "../config";
import { levelEnUnitMap } from "../gameData";
import { GSprite } from "src/shared/phaser-util";

export function drawSolution(
  game: Phaser.Game,
  gameRefs: GameRefs,
  levelId: string,
) {
  const solId = gameRefs.saveFile.activeSolutions[levelId];
  const frUnits = gameRefs.saveFile.levelSolutions[levelId][solId].cardIds;

  frUnits.forEach((cardId, unitIndex) => {
    if (cardId !== undefined) {
      const unit = unitMap[cardId];

      const unitPos = createPosition(
        "left", 250 + 200 * unitIndex, config.levelSelectCardWidth,
        "bot", 750, config.levelSelectCardHeight,
      );
      createUnit(game, gameRefs, unitPos, cardId);

      const unitHpPos = relativeTo(unitPos,
        "below", 50,
        config.unitHpBarWidth, config.unitHpBarHeight,
      );
      unitHpPos.yMax = unitHpPos.yMin + (unitHpPos.yMax - unitHpPos.yMin) * (unit.hp / unit.maxHp);
      createUnitHp(game, gameRefs, unitHpPos, "hp");

      unit.abilities.forEach((ability, abilityIndex) => {
        const abilityPos = relativeTo(unitHpPos,
          "below", 50 + 110 * abilityIndex,
          config.unitHpBarWidth, config.unitHpBarHeight,
        );
        createUnitAbility(game, gameRefs, abilityPos, "ability");
      });
    }
  });

  const enUnits = levelEnUnitMap[levelId];
  enUnits.forEach((cardId, unitIndex) => {
    if (cardId !== undefined) {
      const unit = unitMap[cardId];

      const unitPos = createPosition(
        "left", 1500, config.levelSelectCardWidth,
        "bot", 750, config.levelSelectCardHeight,
      );
      createUnit(game, gameRefs, unitPos, cardId);

      const unitHpPos = relativeTo(unitPos,
        "below", 50,
        config.unitHpBarWidth, config.unitHpBarHeight,
      );
      unitHpPos.yMax = unitHpPos.yMin + (unitHpPos.yMax - unitHpPos.yMin) * (unit.hp / unit.maxHp);
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
  const unit: UnitSprite = gameRefs.gameScreenData.unitPool.getFirstExists(false, true, pos.xMin, pos.yMin, key);
  
  if (unit.data.init === undefined || unit.data.init === false) {

    unit.data.init = true;
  }

  return unit;
}

export function createUnitAbility(
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