import { GameRefs } from "../states/game";
import { createPosition, Position, inPosition, relativeTo } from "../util/position";
import { config } from "../config";
import { levelEnUnitMap } from "../gameData";
import { GSprite } from "src/shared/phaser-util";
import { Ability, HasAbilities, abilityText, intentText } from "src/shared/game/ability";
import { extendSolution, Solution, runSolution } from "src/shared/game/solution";
import { applyScreenEvent } from "../util/screenEvents";
import * as SE from "../util/screenEvents";
import { Location, Tree, drawPositions } from "src/shared/tree";
import { Game, Button } from "phaser-ce";
import { mkGameState, GameState, FrStUnit, EnStUnit } from "src/shared/game/state";
import { Action } from "../../shared/game/action";
import { createButtonInPool, addText } from "../util/btn";
import { TargetType, GlobalId } from "../../shared/game/entityId";
import { OVER, NEUTRAL } from "../util/button";
import { Unit } from "../../shared/game/unit";
import { SpritePool } from "../util/pool";
import { SolInfo } from "../savefile/rep";
import { Intent } from "src/shared/game/intent";
import { AIRoute, routeText } from "src/shared/game/ai";
import { LogKeys } from "src/shared/game/log";

export type StatsScreenData = {
  spriteGroup: Phaser.Group,
  statsLabel?: Phaser.Text,
  abilitiesLabel?: Phaser.Text,
  texts: Phaser.Text[],
  abilitiesPool: SpritePool<AbilitySprite>,
  intentPool: SpritePool<EnIntentSprite>,
  outPool: SpritePool<EnOutSprite>,
  arrowPool: SpritePool<Phaser.Sprite>,
  currRoutePool: SpritePool<Phaser.Sprite>,
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
  // st action log
  solLog.st.forEach((entry, actionIndex) => {
    const actionBtnPos = createPosition(
      "left", 50, config.logButtonWidth,
      "top", 600 + config.logButtonHeight * actionIndex, config.logButtonHeight,
    );
    createActionLogButton(game, gameRefs, entry.action, actionBtnPos, "btn_log", actionIndex, "st");
  });
  // fr action log
  solLog.fr.forEach((entry, actionIndex) => {
    const actionBtnPos = createPosition(
      "left", 50, config.logButtonWidth,
      "top", 700 + solLog.st.length * config.logButtonHeight + config.logButtonHeight * actionIndex, config.logButtonHeight,
    );
    createActionLogButton(game, gameRefs, entry.action, actionBtnPos, "btn_log", actionIndex, "fr");
  });
  // en action log
  solLog.en.forEach((entry, actionIndex) => {
    const actionBtnPos = createPosition(
      "left", 50, config.logButtonWidth,
      "top", 800 + solLog.st.length * config.logButtonHeight + solLog.fr.length * config.logButtonHeight + config.logButtonHeight * actionIndex, config.logButtonHeight,
    );
    createActionLogButton(game, gameRefs, entry.action, actionBtnPos, "btn_log", actionIndex, "en");
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
  gameRefs.gameScreenData.statsScreenData.intentPool.killAll();
  gameRefs.gameScreenData.statsScreenData.outPool.killAll();
  gameRefs.gameScreenData.statsScreenData.arrowPool.killAll();
  gameRefs.gameScreenData.statsScreenData.currRoutePool.killAll();

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
        const frUnit = <FrStUnit>(<any>unit);
        frUnit.abilities.forEach((ability, abilityIndex) => {
          const ablPos = createPosition(
            "left", 2080, config.abilityIconWidth,
            "bot", 400, config.abilityIconHeight - 170 * (abilityIndex),
          );
          const abilityIcon = createUnitAbility(
            game, gameRefs, ablPos, ability.spriteId, ability, id, type);
        });
      } else if (type === "enemy") {
        const enUnit = <EnStUnit>(<any>unit);
        // draw AI intents
        enUnit.ai.forEach((ai, aiIndex) => {
          const intentPos = createPosition(
            "left", 2080, config.abilityIconWidth,
            "bot", 400 - 170 * aiIndex, config.abilityIconHeight,
          );
          const intentIcon = createEnIntent(
            game, gameRefs, intentPos, ai.spriteId, ai.intent, id, type);

          // draw AI routing
          ai.outs.forEach((route, outIndex) => {
            const routePos = createPosition(
              "left", 2280, 40,
              "bot", 400 - 170 * aiIndex - 50 * outIndex + 100, 40,
            );
            const routeIcon = createEnOut(
              game, gameRefs, routePos, route, id, type
            );
          });
          
          // draw current intent arrow
          if (aiIndex === enUnit.currentAI) {
            const currRoutePos = createPosition(
              "left", 1980, 75,
              "bot", 400 - 170 * aiIndex, 75,
            );
            gameRefs.gameScreenData.statsScreenData.currRoutePool.getFirstExists(
              false, true, currRoutePos.xMin, currRoutePos.yMin, "current_route"
            );
          }
        });
      }
    }
  }
}

type AbilitySprite = GSprite<{
  selecting: boolean,
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
  const sprite: AbilitySprite = createButtonInPool(
    game,
    gameRefs.gameScreenData.statsScreenData.abilitiesPool,
    pos,
    { ability, id, type },
    key,
    undefined,
    // onInputDown
    () => {
      //
    },
    // onInputUp
    () => {
      if (sprite.data.ability.inputs.length === 0) {
        applyScreenEvent(new SE.ExtendLevelSolution({
          ability: sprite.data.ability,
          origin: new GlobalId(sprite.data.id, sprite.data.type),
          inputs: [],
        }), game, gameRefs);
      } else {
        applyScreenEvent(new SE.SetClickState({
          ability: sprite.data.ability,
          currentInputs: [],
          origin: new GlobalId(sprite.data.id, sprite.data.type),
        }), game, gameRefs);
      }
    },
    // onInputOver
    () => {
      //
    },
    // onInputOut
    () => {
      //
    },
    // popupF
    (self: AbilitySprite) => {
      const hoverPos = relativeTo(pos,
        "right", 50,
        1000, 100,
      );
      const sprite = game.add.sprite(hoverPos.xMin, hoverPos.yMin, "bg_hover_2");
      addText(game, sprite, hoverPos, `${abilityText(self.data.ability)}`, "#FF0000", 50);
      return sprite;
    },
  );

  return sprite;
}

type EnIntentSprite = GSprite<{
  selecting: boolean,
  init: boolean,
  intent: Intent,
  id: number,
  type: TargetType,
}>;

export function createEnIntent(
  game: Phaser.Game,
  gameRefs: GameRefs,
  pos: Position,
  key: string,
  intent: Intent,
  id: number,
  type: TargetType,
): EnIntentSprite {
  const sprite: EnIntentSprite = createButtonInPool(
    game,
    gameRefs.gameScreenData.statsScreenData.intentPool,
    pos,
    { intent, id, type },
    key,
    undefined,
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
    // popupF
    (self: EnIntentSprite) => {
      const hoverPos = relativeTo(pos,
        "right", 50,
        1000, 100,
      );
      const sprite = game.add.sprite(hoverPos.xMin, hoverPos.yMin, "bg_hover_2");
      addText(game, sprite, hoverPos, `${intentText(self.data.intent)}`, "#FF0000", 50);
      return sprite;
    },
  );

  return sprite;
}

type EnOutSprite = GSprite<{
  selecting: boolean,
  init: boolean,
  route: AIRoute,
  id: number,
  type: TargetType,
}>;

export function createEnOut(
  game: Phaser.Game,
  gameRefs: GameRefs,
  pos: Position,
  route: AIRoute,
  id: number,
  type: TargetType,
): EnOutSprite {
  const sprite: EnOutSprite = createButtonInPool(
    game,
    gameRefs.gameScreenData.statsScreenData.intentPool,
    pos,
    { route, id, type },
    "route",
    undefined,
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
    // popupF
    (self: EnOutSprite) => {
      const hoverPos = relativeTo(pos,
        "right", 50,
        1000, 100,
      );
      const sprite = game.add.sprite(hoverPos.xMin, hoverPos.yMin, "bg_hover_2");
      addText(game, sprite, hoverPos, `${routeText(self.data.route)}`, "#FF0000", 50);
      return sprite;
    },
  );

  return sprite;
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
  index: number,
  type: LogKeys,
  btnText: Phaser.Text,
}>;

export function createActionLogButton(
  game: Phaser.Game,
  gameRefs: GameRefs,
  action: Action,
  pos: Position,
  key: string,
  index: number,
  type: LogKeys,
): ActionLogButton {
  const frame: number = 0;
  const txtColor: string = "#FF0000";
  
  const btn = createButtonInPool(
    game,
    gameRefs.gameScreenData.logBtnPool,
    pos,
    { action, index, type },
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
      applyScreenEvent(new SE.ShowIntermediateSol(btn.data.index, btn.data.type), game, gameRefs);
    },
    // onInputOut
    () => {
      btn.frame = NEUTRAL;
      applyScreenEvent(new SE.ClearIntermediateSol(), game, gameRefs);
    },
  );

  const btnString = action.tag;
  return addText(game, btn, pos, btnString, txtColor, 50);
}