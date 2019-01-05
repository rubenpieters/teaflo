import { focus, over, set } from "src/shared/iassign-util";
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
import { mkGameState, EnStUnit, FrStUnit, filteredFr, filteredEn, GameState } from "src/shared/game/state";
import { Action, actionText } from "../../shared/game/action";
import { createButtonInPool, addText, ButtonValues } from "../util/btn";
import { UnitType, PositionId, toPositionId, TargetId } from "../../shared/game/entityId";
import { Log, LogEntry, LogKeys } from "../../shared/game/log";
import { triggerSprite, Trigger, TriggerLog, StTrigger, triggerOrder } from "../../shared/game/trigger";
import { spriteMap } from "../../shared/data/units/spriteMap";

export type IntermediateSol = {
  index: number,
  type: LogKeys,
}

function pickIntermediateSol(
  intermediateSol: IntermediateSol,
  log: Log,
): LogEntry {
  return log[intermediateSol.type][intermediateSol.index];
}

export function drawSolutionRep(
  game: Phaser.Game,
  gameRefs: GameRefs,
  levelId: string,
  intermediateSol?: IntermediateSol,
) {
  gameRefs.gameScreenData.unitPool.killAll();
  gameRefs.gameScreenData.unitHpPool.killAll();
  gameRefs.gameScreenData.unitTriggerPool.killAll();
  gameRefs.gameScreenData.unitAbilityPool.killAll();
  gameRefs.gameScreenData.intermediateActionTexts.forEach(x => x.destroy());

  const solId = gameRefs.saveFile.activeSolutions[levelId];
  const sol = gameRefs.saveFile.levelSolutions[levelId][solId];

  // make initial state
  // TODO: move to applyScreenEvent?
  const frUnits = gameRefs.saveFile.levelSolutions[levelId][solId].deploy;
  const enUnits = levelEnUnitMap[levelId];
  const initState = mkGameState(frUnits, enUnits);
  const solResult = runSolution(sol.solution, sol.loc, initState);
  let solState = solResult.state;
  let intermediateAction: Action | undefined = undefined;
  let intermediateTransforms: TriggerLog[] | undefined = undefined;
  if (intermediateSol !== undefined) {
    const sol = pickIntermediateSol(intermediateSol, solResult.log);
    solState = sol.state;
    intermediateAction = sol.action;
    intermediateTransforms = sol.transforms;
  }
  gameRefs.gameScreenData.state = solState;

  // mark this solution with a win
  if (solResult.win) {
    const activeLevel = gameRefs.saveFile.activeSolutions[levelId];
    gameRefs.saveFile = focus(gameRefs.saveFile,
      set(x => x.levelSolutions[levelId][activeLevel].solution.win, true)
    );
  }

  // calculate max threat value
  const enIds = filteredEn(solState)
    .map(x => x.id)
    ;
  const maxThreat = filteredFr(solState)
    .map(x => Object.values(x.threatMap))
    .reduce((acc, curr) => Math.max(...curr.concat(acc)), 1)
    ;

  // draw friendly units
  solState.frUnits.forEach((unit, unitIndex) => {
    if (unit !== undefined) {

      const unitPos = createPosition(
        "left", 1050 + 200 * unitIndex, config.levelSelectCardWidth,
        "top", 600, config.levelSelectCardHeight,
      );
      createUnit(game, gameRefs, unitPos, unit.cardId, unitIndex, "friendly");

      // HP
      const unitHpPos = relativeTo(unitPos,
        "below", 50,
        config.unitHpBarWidth, config.unitHpBarHeight,
      );
      unitHpPos.xMax = unitHpPos.xMin + (unitHpPos.xMax - unitHpPos.xMin) * (unit.hp / unit.maxHp);
      createUnitResource(game, gameRefs, unitHpPos, "hp");

      // CH
      const unitChPos = relativeTo(unitPos,
        "below", 150,
        config.unitHpBarWidth, config.unitHpBarHeight,
      );
      unitChPos.xMax = unitChPos.xMin + (unitChPos.xMax - unitChPos.xMin) * (unit.charges / unit.maxCharges);
      createUnitResource(game, gameRefs, unitChPos, "ch");

      // TH
      let i = 0;
      for (const enId of enIds) {
        const unitThPos = relativeTo(unitPos,
          "below", 250 + 100 * i,
          config.unitHpBarWidth, config.unitHpBarHeight,
        );
        unitThPos.xMax = unitThPos.xMin + (unitThPos.xMax - unitThPos.xMin) * (unit.threatMap[enId] / maxThreat);
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

      // HP
      const unitHpPos = relativeTo(unitPos,
        "below", 50,
        config.unitHpBarWidth, config.unitHpBarHeight,
      );
      unitHpPos.xMax = unitHpPos.xMin + (unitHpPos.xMax - unitHpPos.xMin) * (unit.hp / unit.maxHp);
      createUnitResource(game, gameRefs, unitHpPos, "hp");

      // CH
      const unitChPos = relativeTo(unitPos,
        "below", 150,
        config.unitHpBarWidth, config.unitHpBarHeight,
      );
      unitChPos.xMax = unitChPos.xMin + (unitChPos.xMax - unitChPos.xMin) * (unit.charges / unit.maxCharges);
      createUnitResource(game, gameRefs, unitChPos, "ch");
    }

    // draw triggers
    triggerOrder.forEach((group, groupIndex) => {
      solState.triggers[group].forEach((trigger, triggerIndex) => {
        const trPos = createPosition(
          "left", 1050 + 50 * triggerIndex, config.triggerWidth,
          "top", 1250 + 100 * groupIndex, config.triggerHeight,
        );
        createUnitTrigger(game, gameRefs, trPos, trigger);
      })
    });
  });

  // draw intermediate action if defined
  if (intermediateAction !== undefined) {
    drawAction(game, gameRefs, solState, intermediateAction);
  }

  // draw intermediate transforms if defined and not empty
  if (intermediateTransforms !== undefined && intermediateTransforms.length !== 0) {
    intermediateTransforms.forEach((triggerLog, triggerIndex) => {
      const lblPos = createPosition(
        "right", 500, 150,
        "top", 200 + 75 * triggerIndex, 70,
      );
      const lbl = game.add.text(
        lblPos.xMin, lblPos.yMin, `${triggerLog.tag}: ${actionText(triggerLog.before)}->${actionText(triggerLog.after)}`, {
          fill: "#333333",
          fontSize: 70,
          boundsAlignH: "center",
          boundsAlignV: "middle",
        }
      );
      lbl.setTextBounds(0, 0, lblPos.xMax - lblPos.xMin, lblPos.yMax - lblPos.yMin);
      gameRefs.gameScreenData.intermediateActionTexts.push(lbl);
    });
  }
}

function drawAction(
  game: Game,
  gameRefs: GameRefs,
  state: GameState,
  action: Action,
) {
  switch (action.tag) {
    case "CombinedAction": {
      return;
    }
    case "Damage": {
      const lblPos = targetIdToLocation(state, action.target);
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
      const lblPos = targetIdToLocation(state, action.target);
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
      const lblPos = targetIdToLocation(state, action.target);
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
      const lblPos = targetIdToLocation(state, action.toFriendly);
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
    case "AddTrigger": {
      const lblPos = targetIdToLocation(state, action.target);
      const lbl = game.add.text(
        lblPos.xMin, lblPos.yMin, `+${action.trigger.fragments} ${action.trigger.tag}`, {
          fill: "#333333",
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

function targetIdToLocation(
  state: GameState,
  targetId: TargetId,
): Position {
  switch (targetId.type) {
    case "enemy": {
      const posId = toPositionId(state, targetId);
      const lblPos = createPosition(
        "left", 1250 + 1050 + 200 * posId.id, 150,
        "top", 750, 70,
      );
      return lblPos
    }
    case "friendly": {
      const posId = toPositionId(state, targetId);
      const lblPos = createPosition(
        "left", 1050 + 200 * posId.id, 150,
        "top", 750, 70,
      );
      return lblPos
    }
    case "status": {
      // TODO: find exact position of trigger
      const lblPos = createPosition(
        "left", 200, 150,
        "top", 750, 70,
      );
      return lblPos
    }
  }
}

type UnitSprite = GSprite<ButtonValues & {
  id: number,
  type: UnitType,
}>;

export function createUnit(
  game: Phaser.Game,
  gameRefs: GameRefs,
  pos: Position,
  key: string,
  id: number,
  type: UnitType,
): UnitSprite {
  const unit: UnitSprite = createButtonInPool(
    game,
    gameRefs.gameScreenData.unitPool,
    pos,
    { id, type },
    spriteMap[key],
    {
      clickLeft: () => {
        if (gameRefs.gameScreenData.clickState !== undefined) {
          applyScreenEvent(new SE.AdvanceClickState(new PositionId(unit.data.id, unit.data.type)), game, gameRefs);
        } else {
          applyScreenEvent(new SE.LockCardInfo(unit.data.id, unit.data.type), game, gameRefs);
        }
      },
      hoverOver: () => {
        applyScreenEvent(new SE.ShowCardInfo(unit.data.id, unit.data.type), game, gameRefs);
      },
      hoverOut: () => {
        applyScreenEvent(new SE.ClearCardInfo(), game, gameRefs);
      },
    }
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

type TriggerSprite = GSprite<ButtonValues & {
  trigger: StTrigger,
}>;

export function createUnitTrigger(
  game: Phaser.Game,
  gameRefs: GameRefs,
  pos: Position,
  trigger: StTrigger,
): TriggerSprite {
  const sprite: TriggerSprite = createButtonInPool(
    game,
    gameRefs.gameScreenData.unitTriggerPool,
    pos,
    { trigger },
    triggerSprite(trigger),
    {
      clickLeft: () => {
        if (gameRefs.gameScreenData.clickState !== undefined) {
          console.log(`TRIGGER ID: ${sprite.data.trigger.id}`);
          //applyScreenEvent(new SE.AdvanceClickState(new PositionId(sprite.data.trigger.id, unit.data.type)), game, gameRefs);
        }
      },
      popupSprite: (self: TriggerSprite) => {
        const hoverPos = relativeTo(pos,
          "right", 50,
          1000, 100,
        );
        const sprite = game.add.sprite(hoverPos.xMin, hoverPos.yMin, "bg_hover_2");
        addText(game, sprite, hoverPos, `(${self.data.trigger.owner.id}) ${self.data.trigger.id}: ${self.data.trigger.tag} ${self.data.trigger.fragments}`, "#FF0000", 50);
        return sprite;
      },
    }
  );

  return sprite;
}