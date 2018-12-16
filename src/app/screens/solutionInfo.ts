import { GameRefs } from "../states/game";
import { createPosition, Position, inPosition } from "../util/position";
import { config } from "../config";
import { levelEnUnitMap } from "../gameData";
import { GSprite } from "src/shared/phaser-util";
import { Ability, HasAbilities } from "src/shared/game/ability";
import { extendSolution, Solution, runSolution } from "src/shared/game/solution";
import { applyScreenEvent } from "../util/screenEvents";
import * as SE from "../util/screenEvents";
import { Location, Tree, drawPositions } from "src/shared/tree";
import { Game, Button } from "phaser-ce";
import { mkGameState, GameState } from "src/shared/game/state";
import { Action } from "../../shared/game/action";
import { createButtonInPool, addText } from "../util/btn";
import { TargetType, GlobalId } from "../../shared/game/entityId";
import { OVER, NEUTRAL } from "../util/button";
import { Unit } from "../../shared/game/unit";
import { SpritePool } from "../util/pool";
import { SolInfo } from "../savefile/rep";

export type StatsScreenData = {
  spriteGroup: Phaser.Group,
  statsLabel?: Phaser.Text,
  abilitiesLabel?: Phaser.Text,
  texts: Phaser.Text[],
  abilitiesPool: SpritePool<AbilitySprite>,
  arrowPool: SpritePool<Phaser.Sprite>,
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
  mkTree(game, gameRefs, sol);

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
    gameRefs.gameScreenData.statsScreenData.statsLabel = statsLbl;
  }
  if (gameRefs.gameScreenData.statsScreenData.abilitiesLabel === undefined) {
    const ablsLblPos = createPosition(
      "left", 1720, 720,
      "bot", 540, 100,
    );
    const ablsLbl = game.add.text(
      ablsLblPos.xMin, ablsLblPos.yMin, "Abilities", {
        fill: "#000000",
        fontSize: 70,
        boundsAlignH: "center",
        boundsAlignV: "middle",
      }
    );
    ablsLbl.setTextBounds(0, 0, ablsLblPos.xMax - ablsLblPos.xMin, ablsLblPos.yMax - ablsLblPos.yMin);
    gameRefs.gameScreenData.statsScreenData.abilitiesLabel = ablsLbl;
  }
}

export function drawCardInfo(
  game: Phaser.Game,
  gameRefs: GameRefs,
  state: GameState,
) {
  gameRefs.gameScreenData.statsScreenData.texts.forEach(x => x.destroy());
  gameRefs.gameScreenData.statsScreenData.texts = [];
  gameRefs.gameScreenData.statsScreenData.abilitiesPool.killAll();
  gameRefs.gameScreenData.statsScreenData.arrowPool.killAll();

  // draw selection arrow for locked
  const lockInfo = gameRefs.gameScreenData.lockInfo;
  if (lockInfo !== undefined) {
    const id = lockInfo.id;
    const arrowPos: Position = lockInfo.type === "friendly"
    ? createPosition(
      "left", 1050 + 200 * id, config.selectArrowWidth,
      "top", 450, config.selectArrowHeight,
    )
    : createPosition(
      "left", 2300 + 200 * id, config.selectArrowWidth,
      "top", 450, config.selectArrowHeight,
    );
    gameRefs.gameScreenData.statsScreenData.arrowPool.getFirstExists(
      false, true, arrowPos.xMin, arrowPos.yMin, "arrow", 1
    );
  }

  // show hover info, if not available show lock info, if not available show nothing
  const info = gameRefs.gameScreenData.hoverInfo !== undefined
    ? gameRefs.gameScreenData.hoverInfo
    : gameRefs.gameScreenData.lockInfo !== undefined
    ? gameRefs.gameScreenData.lockInfo : undefined;
  if (info !== undefined) {
    const type = info.type;
    const id = info.id;
    let arr: (Unit | undefined)[];
    if (type === "friendly") {
      arr = state.frUnits;
    } else {
      arr = state.enUnits;
    }
    const unit = arr[id];
    if (unit !== undefined) {
      // draw selection arrow for hover
      // only if lock info is not equal to hover info
      if (!(lockInfo !== undefined && info.id === lockInfo.id && info.type === lockInfo.type)) {
        const arrowPos: Position = type === "friendly"
        ? createPosition(
          "left", 1050 + 200 * id, config.selectArrowWidth,
          "top", 450, config.selectArrowHeight,
        )
        : createPosition(
          "left", 2300 + 200 * id, config.selectArrowWidth,
          "top", 450, config.selectArrowHeight,
        );
        gameRefs.gameScreenData.statsScreenData.arrowPool.getFirstExists(
          false, true, arrowPos.xMin, arrowPos.yMin, "arrow", 0
        );
      }
  
      // draw stats text
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
  
      if (type === "friendly") {
        const frUnit = <HasAbilities>(<any>unit);
        frUnit.abilities.forEach((ability, abilityIndex) => {
          const ablPos = createPosition(
            "left", 2080, config.abilityIconWidth,
            "bot", 400, config.abilityIconHeight - 170 * (abilityIndex),
          );
          const abilityIcon = createUnitAbility(
            game, gameRefs, ablPos, ability.spriteId, ability, id, type);
        });
      }
    }
  }
}

type AbilitySprite = GSprite<{
  init: boolean,
  ability: Ability,
  id: number,
  type: TargetType,
}>;

export function createUnitAbility(
  game: Phaser.Game,
  gameRefs: GameRefs,
  pos: Position,
  key: string,
  ability: Ability,
  id: number,
  type: TargetType,
): AbilitySprite {
  const unit: AbilitySprite =
    gameRefs.gameScreenData.statsScreenData.abilitiesPool.getFirstExists(false, true, pos.xMin, pos.yMin, key);
  
  unit.data.ability = ability;
  unit.data.id = id;
  unit.data.type = type;

  if (unit.data.init === undefined || unit.data.init === false) {
    unit.inputEnabled = true;
    unit.events.onInputUp.add(() => {
      if (
        inPosition(pos, game.input.activePointer.x, game.input.activePointer.y)
      ) {
        if (unit.data.ability.inputs.length === 0) {
          applyScreenEvent(new SE.ExtendLevelSolution({
            ability: unit.data.ability,
            origin: new GlobalId(unit.data.id, unit.data.type),
            inputs: [],
          }), game, gameRefs);
        } else {
          applyScreenEvent(new SE.SetClickState({
            ability, currentInputs: [], origin: new GlobalId(unit.data.id, unit.data.type),
          }), game, gameRefs);
        }
      }
    });

    unit.data.init = true;
  }

  return unit;
}

function mkTree(
  game: Game,
  gameRefs: GameRefs,
  solInfo: SolInfo,
) {
  // clear old
  gameRefs.gameScreenData.solTreePool.forEach(x => x.destroy());

  const x = 220;
  const y = 100;

  // create (0,0) element
  const sprite: Phaser.Graphics = game.add.graphics(x - 50, y, gameRefs.gameScreenData.spriteGroup);
  if (solInfo.loc.length === 0) {
    sprite.beginFill(0xFF77CC);
  } else {
    sprite.beginFill(0x4477CC);
  }
  sprite.drawRect(0, 0, 40, 40);
  sprite.endFill();
  sprite.inputEnabled = true;
  sprite.events.onInputDown.add((obj: any, pointer: Phaser.Pointer) => {
    if (pointer.leftButton.isDown) {
      applyScreenEvent(new SE.ChangeTreeLoc([]), game, gameRefs);
    } else if (pointer.rightButton.isDown) {
      applyScreenEvent(new SE.CutTreeLoc([]), game, gameRefs);
    }
  });

  // create other elements
  const drawPosList = drawPositions(solInfo.solution.tree);

  const sprites: Phaser.Graphics[] = [sprite];
  for (const drawPos of drawPosList) {
    const sprite: Phaser.Graphics = game.add.graphics(x + drawPos.x * 50, y + drawPos.y * 50, gameRefs.gameScreenData.spriteGroup);
    if (solInfo.loc.toString() === drawPos.loc.toString()) {
      sprite.beginFill(0xFF77CC);
    } else {
      sprite.beginFill(0x4477CC);
    }
    sprite.drawRect(0, 0, 40, 40);
    sprite.endFill();
    sprite.inputEnabled = true;
    sprite.events.onInputDown.add((obj: any, pointer: Phaser.Pointer) => {
      if (pointer.leftButton.isDown) {
        applyScreenEvent(new SE.ChangeTreeLoc(drawPos.loc), game, gameRefs);
      } else if (pointer.rightButton.isDown) {
        applyScreenEvent(new SE.CutTreeLoc(drawPos.loc), game, gameRefs);
      }
    });
    sprites.push(sprite);
  }

  gameRefs.gameScreenData.solTreePool = sprites;
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