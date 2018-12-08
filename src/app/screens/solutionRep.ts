import { GameRefs } from "../states/game";
import { createPosition, relativeTo, Position, inPosition } from "../util/position";
import { config } from "../config";
import { levelEnUnitMap } from "../gameData";
import { GSprite } from "src/shared/phaser-util";
import { Ability } from "src/shared/game/ability";
import { extendSolution, Solution, runSolution } from "src/shared/game/solution";
import { applyScreenEvent } from "../util/screenEvents";
import * as SE from "../util/screenEvents";
import { Location, Tree } from "src/shared/tree";
import { Game, Button } from "phaser-ce";
import { mkGameState } from "src/shared/game/state";
import { Action } from "../../shared/game/action";
import { createButtonInPool, addText } from "../util/btn";
import { TargetType, PositionId } from "../../shared/game/entityId";
import { Log, LogEntry } from "../../shared/game/log";

export type IntermediateSol = {
  index: number,
}

function pickIntermediateSol(
  intermediateSol: IntermediateSol,
  log: Log,
): LogEntry {
  return log[intermediateSol.index];
}

export function drawSolutionRep(
  game: Phaser.Game,
  gameRefs: GameRefs,
  levelId: string,
  intermediateSol?: IntermediateSol,
) {
  gameRefs.gameScreenData.unitPool.killAll();
  gameRefs.gameScreenData.unitHpPool.killAll();
  gameRefs.gameScreenData.unitAbilityPool.killAll();

  const solId = gameRefs.saveFile.activeSolutions[levelId];
  const sol = gameRefs.saveFile.levelSolutions[levelId][solId];

  // make initial state
  // TODO: move to applyScreenEvent?
  const frUnits = gameRefs.saveFile.levelSolutions[levelId][solId].cardIds;
  const enUnits = levelEnUnitMap[levelId];
  const initState = mkGameState(frUnits, enUnits);
  const solResult = runSolution(sol.solution, sol.loc, initState);
  let solState = solResult.state;
  if (intermediateSol !== undefined) {
    solState = pickIntermediateSol(intermediateSol, solResult.log).state;
  }
  gameRefs.gameScreenData.state = solState;

  // draw friendly units
  solState.frUnits.forEach((unit, unitIndex) => {
    if (unit !== undefined) {

      const unitPos = createPosition(
        "left", 1050 + 200 * unitIndex, config.levelSelectCardWidth,
        "top", 600, config.levelSelectCardHeight,
      );
      createUnit(game, gameRefs, unitPos, unit.cardId, unitIndex, "friendly");

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
        "left", 2300 + 200 * unitIndex, config.levelSelectCardWidth,
        "top", 600, config.levelSelectCardHeight,
      );
      createUnit(game, gameRefs, unitPos, unit.cardId, unitIndex, "enemy");

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
  selecting: boolean,
  id: number,
  type: TargetType,
}>;

export function createUnit(
  game: Phaser.Game,
  gameRefs: GameRefs,
  pos: Position,
  key: string,
  id: number,
  type: TargetType,
): UnitSprite {
  const unit: UnitSprite = createButtonInPool(
    game,
    gameRefs.gameScreenData.unitPool,
    pos,
    { id, type },
    key,
    undefined,
    // onInputDown
    () => {
      //
    },
    // onInputUp
    () => {
      if (gameRefs.gameScreenData.clickState !== undefined) {
        applyScreenEvent(new SE.AdvanceClickState(new PositionId(unit.data.id, unit.data.type)), game, gameRefs);
      } else {
        applyScreenEvent(new SE.LockCardInfo(unit.data.id, unit.data.type), game, gameRefs);
      }
    },
    // onInputOver
    () => {
      applyScreenEvent(new SE.ShowCardInfo(unit.data.id, unit.data.type), game, gameRefs);
    },
    // onInputOut
    () => {
      applyScreenEvent(new SE.ClearCardInfo(), game, gameRefs);
    },
  );

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
        if (unit.data.ability.inputs.length === 0) {
          applyScreenEvent(new SE.ExtendLevelSolution({
            ability: unit.data.ability,
            inputs: []
          }, unit.data.levelId), game, gameRefs);
        } else {
          applyScreenEvent(new SE.SetClickState({ ability, currentInputs: [] }), game, gameRefs);
        }
      }
    });

    unit.data.init = true;
  }

  return unit;
}