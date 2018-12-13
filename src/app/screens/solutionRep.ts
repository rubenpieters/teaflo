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
  gameRefs.gameScreenData.intermediateActionTexts.forEach(x => x.destroy());

  const solId = gameRefs.saveFile.activeSolutions[levelId];
  const sol = gameRefs.saveFile.levelSolutions[levelId][solId];

  // make initial state
  // TODO: move to applyScreenEvent?
  const frUnits = gameRefs.saveFile.levelSolutions[levelId][solId].cardIds;
  const enUnits = levelEnUnitMap[levelId];
  const initState = mkGameState(frUnits, enUnits);
  const solResult = runSolution(sol.solution, sol.loc, initState);
  let solState = solResult.state;
  let intermediateAction: Action | undefined = undefined;
  if (intermediateSol !== undefined) {
    const sol = pickIntermediateSol(intermediateSol, solResult.log);
    solState = sol.state;
    intermediateAction = sol.action;
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
      createUnitResource(game, gameRefs, unitHpPos, "hp");

      const unitChPos = relativeTo(unitPos,
        "below", 150,
        config.unitHpBarWidth, config.unitHpBarHeight,
      );
      unitChPos.xMax = unitChPos.xMin + (unitChPos.xMax - unitChPos.xMin) * (unit.charges / unit.maxCharges);
      createUnitResource(game, gameRefs, unitChPos, "ch");

      let i = 0;
      for (const enId in unit.threatMap) {
        const unitThPos = relativeTo(unitPos,
          "below", 250 + 100 * i,
          config.unitHpBarWidth, config.unitHpBarHeight,
        );
        unitThPos.xMax = unitThPos.xMin + (unitThPos.xMax - unitThPos.xMin) * (unit.threatMap[enId] / 100);
        createUnitResource(game, gameRefs, unitThPos, "th");
        i += 1;
      }
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
      createUnitResource(game, gameRefs, unitHpPos, "hp");

      const unitChPos = relativeTo(unitPos,
        "below", 150,
        config.unitHpBarWidth, config.unitHpBarHeight,
      );
      unitChPos.xMax = unitChPos.xMin + (unitChPos.xMax - unitChPos.xMin) * (unit.charges / unit.maxCharges);
      createUnitResource(game, gameRefs, unitChPos, "ch");
    }
  });

  // draw intermediate action if defined
  if (intermediateAction !== undefined) {
    drawAction(game, gameRefs, intermediateAction);
  }
}

function drawAction(
  game: Game,
  gameRefs: GameRefs,
  action: Action,
) {
  switch (action.tag) {
    case "CombinedAction": {
      return;
    }
    case "Damage": {
      const offset = action.target.type === "friendly" ? 0 : 1250;
      const lblPos = createPosition(
        "left", offset + 1050 + 200 * action.target.id, 150,
        "top", 750, 70,
      );
      const lbl = game.add.text(
        lblPos.xMin, lblPos.yMin, `-${action.value} HP`, {
          fill: "#FF0000",
          fontSize: 70,
          boundsAlignH: "center",
          boundsAlignV: "middle",
        }
      );
      lbl.setTextBounds(0, 0, lblPos.xMax - lblPos.xMin, lblPos.yMax - lblPos.yMin);
      gameRefs.gameScreenData.intermediateActionTexts.push(lbl);
      return;
    }
    case "Heal": {
      const offset = action.target.type === "friendly" ? 0 : 1250;
      const lblPos = createPosition(
        "left", offset + 1050 + 200 * action.target.id, 150,
        "top", 750, 70,
      );
      const lbl = game.add.text(
        lblPos.xMin, lblPos.yMin, `+${action.value} HP`, {
          fill: "#00FF00",
          fontSize: 70,
          boundsAlignH: "center",
          boundsAlignV: "middle",
        }
      );
      lbl.setTextBounds(0, 0, lblPos.xMax - lblPos.xMin, lblPos.yMax - lblPos.yMin);
      gameRefs.gameScreenData.intermediateActionTexts.push(lbl);
      return;
    }
    case "UseCharge": {
      const offset = action.target.type === "friendly" ? 0 : 1250;
      const lblPos = createPosition(
        "left", offset + 1050 + 200 * action.target.id, 150,
        "top", 750, 70,
      );
      const lbl = game.add.text(
        lblPos.xMin, lblPos.yMin, `-${action.value} CH`, {
          fill: "#00AAAA",
          fontSize: 70,
          boundsAlignH: "center",
          boundsAlignV: "middle",
        }
      );
      lbl.setTextBounds(0, 0, lblPos.xMax - lblPos.xMin, lblPos.yMax - lblPos.yMin);
      gameRefs.gameScreenData.intermediateActionTexts.push(lbl);
      return;
    }
    case "AddThreat": {
      const offset = action.toFriendly.type === "friendly" ? 0 : 1250;
      const lblPos = createPosition(
        "left", offset + 1050 + 200 * action.toFriendly.id, 150,
        "top", 750, 70,
      );
      const lbl = game.add.text(
        lblPos.xMin, lblPos.yMin, `+${action.value} TH`, {
          fill: "#000000",
          fontSize: 70,
          boundsAlignH: "center",
          boundsAlignV: "middle",
        }
      );
      lbl.setTextBounds(0, 0, lblPos.xMax - lblPos.xMin, lblPos.yMax - lblPos.yMin);
      gameRefs.gameScreenData.intermediateActionTexts.push(lbl);
      return;
    }
  }
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

export function createUnitResource(
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