import { Pool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { createPosition, relativeTo, Position, center } from "../../util/position";
import { addText, DataSprite, addShader, clearShader } from "../../phaser/datasprite";
import { enFiltered, frFiltered, getTarget, statusById, position } from "../../../shared/game/state";
import { Log, LogEntry, getLogEntry, LogIndex, allLogIndices, logIndexLt, logIndexEq, getPrevLogEntry, LogEntryI, nextLogIndex, StatusLog, splitLog } from "../../../shared/game/log";
import { cardMap } from "../../../app/data/cardMap";
import { TextPool } from "../../phaser/textpool";
import { EntityId, UnitId, friendlyId, TargetId, EnemyId, FriendlyId, StatusId, UnitType } from "../../../shared/definitions/entityId";
import { hoverUnit, clearHover, extendLevelSolution, changeLevelLoc, cutLevelSolution, showChangeUnitScreen, hideChangeUnitScreen, deployUnit } from "./events";
import { chainSpriteCreation, createTween, addTextPopup, speedTypeToSpeed, SpeedType, addSpritePopup, Create, BaseAnimation, SeqAnimation, Animation, ParAnimation, runAsTween, animationDummy } from "../../../app/phaser/animation";
import { drawPositions, Location } from "../../../shared/tree";
import { Solution, SolutionData } from "../../../shared/game/solution";
import { transitionScreen, ScreenCodex } from "../transition";
import { CodexTypes } from "../codex/screen";
import { clearAnimations } from "../util";
import { friendlyUnitPos, enemyUnitPos, statusPos, unitUtilityPositions, explX, explY, explArrowEnd, unitEnMinX, unitFrMinY, logPosition, sourceUnitPos } from "./positions";
import deepEqual from "deep-equal";
import { groupOrder, statusTagDescription } from "../../../shared/game/status";
import { StStatus } from "../../../shared/definitions/statusRow";
import { matchUserInput } from "../../../shared/game/input";
import { aiIndices, indexToAiPos, aiPosToIndex } from "../../../shared/game/ai";
import { GameState } from "../../../shared/definitions/state";
import { FrAbility, EnAbility } from "../../../shared/definitions/unit";
import { Action, ActionWithOrigin } from "../../../shared/definitions/action";
import { UserInput } from "../../../shared/definitions/input";
import { Ability } from "../../../shared/definitions/ability";
import { StatusTag } from "../../../shared/definitions/status";
import { groupFromDesc } from "../../util/description";
import { actionDescription, actionTargets } from "../../../shared/game/action";
import { abilityDescription } from "../../../shared/game/ability";
import { CardId } from "../../../shared/data/cardId";
import { drawLine } from "../../phaser/line";
import { FrUnitId } from "../../../shared/data/frUnitMap";
import { selectedLevelId } from "../../data/saveData";
import { levelData } from "../../data/levelData";
import { range0 } from "../../util/util"

export class ExecScreen {
  clearBtnPool: Pool<{}, {}>
  unitPool: Pool<UnitData, {}>
  emptySlotPool: Pool<EmptySlotData, {}>
  unitResPool: Pool<UnitResData, {}>
  abilityPool: Pool<AbilityData, {}>
  unitTextPool: TextPool
  statsTextPool: TextPool
  stateTextPool: TextPool
  triggerPool: Pool<TriggerData, {}>
  framePool: Pool<FrameData, {}>
  logTextPool: TextPool
  logTextSpritePool: Pool<LogTextSpriteData, {}>
  logActionPool: Pool<LogActionData, {}>
  logTriggerPool: Pool<LogTriggerData, {}>
  solTreePool: Pool<SolTreeData, {}>
  detailBtnPool: Pool<DetailBtnData, {}>
  detailExplPool: Pool<DetailExplData, {}>
  hoverSpritePool: Pool<HoverSpriteData, {}>
  hoverGraphicsPool: Phaser.Graphics
  stateIconPool: Pool<StateIconData, {}>
  logGraphicsPool: Phaser.Graphics
  unitSelectBgPool: Pool<UnitSelectBgData, {}>
  unitSelectPool: Pool<UnitSelectData, {}>
  bgSpritePool: Pool<BgSpriteData, {}>
  intermediateBgPool: Pool<BgSpriteData, {}>
  actionBgPool: Pool<ActionBgData, {}>
  

  animControlBtnPool: Pool<AnimControlBtn, {}>
  treeControlBtnPool: Pool<TreeControlBtn, {}>
  switchStatusOrderBtnPool: Pool<SwitchStatusOrderBtnData, {}>

  state: GameState | undefined
  statusesById: { fr: StStatus[][], en: StStatus[][], } | undefined
  log: Log | undefined
  hoveredUnit: TargetId | undefined
  clickState: { ability: FrAbility, inputs: any[], origin: UnitId, index: number } | undefined
  intermediate: LogIndex | undefined
  statusOrder: "byOrder" | "byId" = "byId"
  selecting: number | undefined
  interactionEnabled: boolean = true

  treeCtrl: "remove" | undefined

  sourceUnit: UnitId | undefined;
  displayedAbility: Ability | undefined;

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.clearBtnPool = mkClearBtnPool(gameRefs);
    this.unitPool = mkUnitPool(gameRefs);
    this.emptySlotPool = mkEmptySlotPool(gameRefs);
    this.unitResPool = mkUnitResPool(gameRefs);
    this.unitTextPool = new TextPool(gameRefs.game);
    this.statsTextPool = new TextPool(gameRefs.game);
    this.stateTextPool = new TextPool(gameRefs.game);
    this.abilityPool = mkAbilityPool(gameRefs);
    this.triggerPool = mkTriggerPool(gameRefs);
    this.framePool = mkFramePool(gameRefs);
    this.logTextPool = new TextPool(gameRefs.game);
    this.logActionPool = mkLogActionPool(gameRefs);
    this.logTriggerPool = mkLogTriggerPool(gameRefs);
    this.animControlBtnPool = mkAnimControlBtnPool(gameRefs);
    this.treeControlBtnPool = mkTreeControlBtnPool(gameRefs);
    this.solTreePool = mkSolTreePool(gameRefs);
    this.detailBtnPool = mkDetailBtnPool(gameRefs);
    this.detailExplPool = mkDetailExplPool(gameRefs);
    this.logTextSpritePool = mkLogTextSpritePool(gameRefs);
    this.hoverSpritePool = mkHoverSpritePool(gameRefs);
    this.hoverGraphicsPool = gameRefs.game.add.graphics();
    this.stateIconPool = mkStateIconPool(gameRefs);
    this.switchStatusOrderBtnPool = mkSwitchStatusOrderBtnPool(gameRefs);
    this.logGraphicsPool = gameRefs.game.add.graphics();
    this.unitSelectBgPool = mkUnitSelectBgPool(gameRefs);
    this.unitSelectPool = mkUnitSelectPool(gameRefs);
    this.bgSpritePool = mkBgSpritePool(gameRefs);
    this.intermediateBgPool = mkIntermediateBgPool(gameRefs);
    this.actionBgPool = mkActionBgDataPool(gameRefs);
  }

  reset() {
    this.state = undefined;
    this.log = undefined;
    this.hoveredUnit = undefined;
    this.clickState = undefined;
    this.intermediate = undefined;
    this.treeCtrl = undefined;
    this.statusOrder = "byOrder";
    this.selecting = undefined;
    this.interactionEnabled = true;
    this.sourceUnit = undefined;
  }

  solDataFromClickState(): SolutionData {
    return {
      ability: this.clickState!.ability.ability,
      origin: this.clickState!.origin,
      inputs: this.clickState!.inputs,
    }
  }

  clearAnimPools() {
    //this.logActionPool.clear();
    this.framePool.clear();
    this.intermediateBgPool.clear();
  }

  canExtendState(): boolean {
    if (this.state === undefined) {
      return false;
    }
    if (this.state.type === "invalid" || this.state.type === "win") {
      return false;
    }
    return true;
  }

  currentState(): GameState {
    if (this.intermediate === undefined) {
      return this.state!;
    }
    return getLogEntry(this.log!, this.intermediate).state;
  }

  drawCurrentState() {
    this.drawState(this.currentState());
    this.drawStats(this.currentState());
  }

  drawClearBtn(
  ) {
    //this.redrawClearBtn();
    //this.clearBtnPool.playIntroAnimations();
  }

  redrawClearBtn(
  ) {
    /*this.clearBtnPool.clear();

    const pos = createPosition(
      "right", 650, 100,
      "bot", 20, 100,
    );
    const sprite = this.clearBtnPool.newSprite(pos.xMin, pos.yMin, "neutral", {});
    addText(this.gameRefs, sprite, pos, "Clear", "#000000", 40);*/
  }

  // DRAW STATE

  drawState(
    state: GameState,
    prevState?: GameState,
  ) {
    this.emptySlotPool.clear();
    this.unitPool.clear();
    this.unitResPool.clear();
    this.triggerPool.clear();
    this.unitTextPool.clear();
    this.hoverSpritePool.clear();
    this.hoverGraphicsPool.clear();
    this.stateTextPool.clear();
    this.abilityPool.clear();
    this.stateIconPool.clear();

    // calculate statuses by id
    this.statusesById = statusById(state);

    // the input type of the click state
    // undefined if the clickstate is undefined
    const currentInputType = this.clickState === undefined ? undefined :
      this.clickState.ability.inputs[this.clickState.inputs.length];

    // calculate max threat value
    const enIds = enFiltered(state).map(x => x.e.id);
    const maxThreat = frFiltered(state)
      .map(x => Object.values(x.e.threatMap))
      .reduce((acc, curr) => Math.max(...curr.concat(acc)), 1)
      ;

    // draw friendly units
    state.frUnits.units.forEach((unit, unitIndex) => {
      if (unit === undefined) {
        const unitPos = friendlyUnitPos(this.gameRefs.settings, state, unitIndex);
        this.emptySlotPool.newSprite(unitPos.xMin, unitPos.yMin, {}, { position: unitIndex });
      } else if (unit !== undefined) {
        const friendlyPos = friendlyUnitPos(this.gameRefs.settings, state, unitIndex);
        const unitPos = deepEqual(this.sourceUnit, unit.id) ?
          sourceUnitPos() :
          friendlyPos;
        const utilityPos = unitUtilityPositions(friendlyPos);
        const unitSprite = this.unitPool.newSprite(unitPos.xMin, unitPos.yMin, {},
          { cardId: unit.cardId,
            globalId: unit.id,
            position: unitIndex,
          },
        );
        if (currentInputType !== undefined && ! matchUserInput(currentInputType, unit.id)) {
          unitSprite.alpha = 0.3;
        } else {
          unitSprite.alpha = 1;
        }

        // hp/ch numbers
        this.stateIconPool.newSprite(utilityPos.hpIconPos.xMin, utilityPos.hpIconPos.yMin, {}, { sprite: "icon_hp.png"});
        this.stateTextPool.newText(utilityPos.hpTextPos, `${unit.hp} / ${unit.maxHp}`, 20);
        this.stateIconPool.newSprite(utilityPos.chIconPos.xMin, utilityPos.chIconPos.yMin, {}, { sprite: "icon_ch.png"});
        this.stateTextPool.newText(utilityPos.chTextPos, `${unit.charges} / ${unit.maxCharges}`, 20);

        // hover bar
        if (
          this.hoveredUnit !== undefined && deepEqual(this.hoveredUnit, unit.id)
        ) {
          const hoverBarPos = relativeTo(friendlyPos,
            [{ type: "above", amt: 5 }, { type: "left", amt: 5 }],
            0, 0,
          );
          const sprite = this.hoverSpritePool.newSprite(hoverBarPos.xMin, hoverBarPos.yMin, {}, {});
          sprite.inputEnabled = false;
        }

        // HP
        const unitHpPos = relativeTo(friendlyPos,
          [{ type: "left", amt: -5 }],
          10, 150,
        );
        const hpHeight = unitHpPos.yMax - unitHpPos.yMin;
        unitHpPos.yMin = unitHpPos.yMin + hpHeight * ((unit.maxHp - unit.hp) / unit.maxHp);
        const unitHpSprite = this.unitResPool.newSprite(unitHpPos.xMin, unitHpPos.yMin, {},
          { cardId: unit.cardId,
            globalId: unit.id,
            type: "hp",
          }
        );
        unitHpSprite.width = 10;
        unitHpSprite.height = hpHeight * (unit.hp / unit.maxHp);

        // CH
        const unitChPos = relativeTo(friendlyPos,
          [{ type: "right", amt: -5 }],
          10, 150,
        );
        const chHeight = unitChPos.yMax - unitChPos.yMin;
        unitChPos.yMin = unitChPos.yMin + chHeight * ((unit.maxCharges - unit.charges) / unit.maxCharges);
        const unitChSprite = this.unitResPool.newSprite(unitChPos.xMin, unitChPos.yMin, {},
          { cardId: unit.cardId,
            globalId: unit.id,
            type: "ch",
          }
        );
        unitChSprite.width = 10;
        unitChSprite.height = chHeight * (unit.charges / unit.maxCharges);

        // TH
        enIds.forEach((enId, enIndex) => {
          const unitThPos = createPosition(this.gameRefs.settings,
            "left", 245 + 170 * unitIndex + 30 * enIndex, 25,
            "top", 740, 50,
          );
          const unitThSprite = this.unitResPool.newSprite(unitThPos.xMin, unitThPos.yMin, {},
            { cardId: unit.cardId,
              globalId: unit.id,
              type: "th",
            }
          );
          const threat = unit.threatMap[enId.id] === undefined ? 0 : unit.threatMap[enId.id];
          unitThSprite.width = 25;
          unitThSprite.height = threat / maxThreat * 50;
          const thTextPos = createPosition(this.gameRefs.settings,
            "left", 245 + 170 * unitIndex + 30 * enIndex, 25,
            "top", 800, 20,
          );
          this.stateTextPool.newText(thTextPos, `${threat}`, 20);
        });

        // abilities
        unit.abilities.forEach((ability, abilityIndex) => {
          if (abilityIndex >= 4) { throw "ability index should be below 4" };
          const flagTop = abilityIndex % 2;
          const flagLeft = abilityIndex < 2 ? 0 : 1;
          const abPos = createPosition(this.gameRefs.settings,
            "left", 250 + 170 * unitIndex + 75 * flagLeft, 70,
            "top", 830 + 75 * flagTop, 70,
          );
          const abilityIcon = this.abilityPool.newSprite(abPos.xMin, abPos.yMin, {},
            { tag: "FrAbilityData", spriteId: ability.spriteId, ability, index: abilityIndex,
              globalId: unit.id
            }
          );
          if (this.clickState !== undefined &&
            deepEqual(this.clickState.origin, unit.id) &&
            abilityIndex === this.clickState.index
          ) {
            addShader(this.gameRefs, abilityIcon, "red-glow");
            //const indicator = this.abilityPool.newSprite(abPos.xMin + 5, abPos.yMin + 5, {}, { tag: "Indicator"});
            //indicator.inputEnabled = false;
          }
        });
      }
    });

    // draw enemy units
    state.enUnits.units.forEach((unit, unitIndex) => {
      if (unit !== undefined) {
        const enemyPos = enemyUnitPos(this.gameRefs.settings, state, unitIndex);
        const unitPos = deepEqual(this.sourceUnit, unit.id) ?
          sourceUnitPos() :
          enemyPos;
        const utilityPos = unitUtilityPositions(enemyPos);
        const unitSprite = this.unitPool.newSprite(unitPos.xMin, unitPos.yMin, {},
          { cardId: unit.cardId,
            globalId: unit.id,
            position: unitIndex,
          },
        );
        if (currentInputType !== undefined && ! matchUserInput(currentInputType, unit.id)) {
          unitSprite.alpha = 0.3;
        } else {
          unitSprite.alpha = 1;
        }

        // hp/ch numbers
        this.stateIconPool.newSprite(utilityPos.hpIconPos.xMin, utilityPos.hpIconPos.yMin, {}, { sprite: "icon_hp.png"});
        this.stateTextPool.newText(utilityPos.hpTextPos, `${unit.hp} / ${unit.maxHp}`, 20);
        this.stateIconPool.newSprite(utilityPos.chIconPos.xMin, utilityPos.chIconPos.yMin, {}, { sprite: "icon_ch.png"});
        this.stateTextPool.newText(utilityPos.chTextPos, `${unit.charges} / ${unit.maxCharges}`, 20);

        // hover bar
        if (
          this.hoveredUnit !== undefined && deepEqual(this.hoveredUnit, unit.id)
        ) {
          const hoverBarPos = relativeTo(enemyPos,
            [{ type: "above", amt: 5 }, { type: "left", amt: 5 }],
            0, 0,
          );
          const sprite = this.hoverSpritePool.newSprite(hoverBarPos.xMin, hoverBarPos.yMin, {}, {});
          sprite.inputEnabled = false;
        }

        // HP
        const unitHpPos = relativeTo(enemyPos,
          [{ type: "left", amt: -5 }],
          10, 150,
        );
        const hpHeight = unitHpPos.yMax - unitHpPos.yMin;
        unitHpPos.yMin = unitHpPos.yMin + hpHeight * ((unit.maxHp - unit.hp) / unit.maxHp);
        const unitHpSprite = this.unitResPool.newSprite(unitHpPos.xMin, unitHpPos.yMin, {},
          { cardId: unit.cardId,
            globalId: unit.id,
            type: "hp",
          }
        );
        unitHpSprite.width = 10;
        unitHpSprite.height = hpHeight * (unit.hp / unit.maxHp);

        // CH
        const unitChPos = relativeTo(enemyPos,
          [{ type: "right", amt: -5 }],
          10, 150,
        );
        const chHeight = unitChPos.yMax - unitChPos.yMin;
        unitChPos.yMin = unitChPos.yMin + chHeight * ((unit.maxCharges - unit.charges) / unit.maxCharges);
        const unitChSprite = this.unitResPool.newSprite(unitChPos.xMin, unitChPos.yMin, {},
          { cardId: unit.cardId,
            globalId: unit.id,
            type: "ch",
          }
        );
        unitChSprite.width = 10;
        unitChSprite.height = chHeight * (unit.charges / unit.maxCharges);

        // show ai
        aiIndices.forEach(index => {
          const ability = unit.abilities[index];
          if (ability !== undefined) {
            const aiPos = indexToAiPos(index);
            const abPos = relativeTo(enemyPos,
              [ { type: "below", amt: 95 + 70 * aiPos.y },
                { type: "left", amt: -45 - 70 * aiPos.x }
              ],
              70, 70,
            );
            this.abilityPool.newSprite(abPos.xMin, abPos.yMin, {},
              { tag: "EnAbilityData", ai: ability, aiIndex: index }
            );
            if (index === aiPosToIndex(unit.aiPosition)) {
              const sprite = this.abilityPool.newSprite(abPos.xMin, abPos.yMin, {},
                { tag: "Indicator" }
              );
              sprite.inputEnabled = false;
            }
          }
        });
      }
    });

    // AETHER
    groupOrder.forEach((tag, tagIndex) => {
      state.statusRows[tag].statuses.forEach((status, statusIndex) => {
        const triggerPos = statusPos(this.gameRefs.settings, state, this.statusesById!, this.statusOrder, status.id, statusIndex, tagIndex);
        const trSprite = this.triggerPool.newSprite(triggerPos.xMin, triggerPos.yMin, {}, { status });
        if (currentInputType !== undefined && ! matchUserInput(currentInputType, status.id)) {
          trSprite.alpha = 0.3;
        } else {
          trSprite.alpha = 1;
        }

        // hover graphics
        if (
          this.hoveredUnit !== undefined && deepEqual(this.hoveredUnit, status.id)
        ) {
          const start = center(triggerPos);
          let end: { x: number, y: number } = undefined as any;
          if (status.owner.type === "friendly") {
            end = center(friendlyUnitPos(this.gameRefs.settings, state, status.owner as EntityId<"friendly">));
          } else if (status.owner.type === "enemy") {
            end = center(enemyUnitPos(this.gameRefs.settings, state, status.owner as EntityId<"enemy">));
          }
          drawLine(this.hoverGraphicsPool, start, end);
        }
      });
    });
  }

  drawBgSprites() {
    this.bgSpritePool.clear();

    const friendlyPos0 = friendlyUnitPos(this.gameRefs.settings, undefined as any, 0);
    this.bgSpritePool.newSprite(friendlyPos0.xMin - 15, friendlyPos0.yMin - 15, {}, { sprite: "bulb_180_180.png" }, /*alpha*/ undefined, /*inputEnabled*/ false);
    const friendlyPos1 = friendlyUnitPos(this.gameRefs.settings, undefined as any, 1);
    this.bgSpritePool.newSprite(friendlyPos1.xMin - 15, friendlyPos1.yMin - 15, {}, { sprite: "bulb_180_180.png" }, /*alpha*/ undefined, /*inputEnabled*/ false);
    const friendlyPos2 = friendlyUnitPos(this.gameRefs.settings, undefined as any, 2);
    this.bgSpritePool.newSprite(friendlyPos2.xMin - 15, friendlyPos2.yMin - 15, {}, { sprite: "bulb_180_180.png" }, /*alpha*/ undefined, /*inputEnabled*/ false);
    const friendlyPos3 = friendlyUnitPos(this.gameRefs.settings, undefined as any, 3);
    this.bgSpritePool.newSprite(friendlyPos3.xMin - 15, friendlyPos3.yMin - 15, {}, { sprite: "bulb_180_180.png" }, /*alpha*/ undefined, /*inputEnabled*/ false);

    const enemyPos0 = enemyUnitPos(this.gameRefs.settings, undefined as any, 0);
    this.bgSpritePool.newSprite(enemyPos0.xMin - 15, enemyPos0.yMin - 15, {}, { sprite: "bulb_180_180.png" }, /*alpha*/ undefined, /*inputEnabled*/ false);
    const enemyPos1 = enemyUnitPos(this.gameRefs.settings, undefined as any, 1);
    this.bgSpritePool.newSprite(enemyPos1.xMin - 15, enemyPos1.yMin - 15, {}, { sprite: "bulb_180_180.png" }, /*alpha*/ undefined, /*inputEnabled*/ false);
    const enemyPos2 = enemyUnitPos(this.gameRefs.settings, undefined as any, 2);
    this.bgSpritePool.newSprite(enemyPos2.xMin - 15, enemyPos2.yMin - 15, {}, { sprite: "bulb_180_180.png" }, /*alpha*/ undefined, /*inputEnabled*/ false);
    const enemyPos3 = enemyUnitPos(this.gameRefs.settings, undefined as any, 3);
    this.bgSpritePool.newSprite(enemyPos3.xMin - 15, enemyPos3.yMin - 15, {}, { sprite: "bulb_180_180.png" }, /*alpha*/ undefined, /*inputEnabled*/ false);

    const statusPosRow0 = statusPos(this.gameRefs.settings, undefined as any, undefined as any, "byOrder", undefined as any, 0, 0);
    this.bgSpritePool.newSprite(statusPosRow0.xMin - 15, statusPosRow0.yMin, {}, { sprite: "vial_720_40.png" }, /*alpha*/ undefined, /*inputEnabled*/ false);
    const statusPosRow1 = statusPos(this.gameRefs.settings, undefined as any, undefined as any, "byOrder", undefined as any, 0, 1);
    this.bgSpritePool.newSprite(statusPosRow1.xMin - 15, statusPosRow1.yMin, {}, { sprite: "vial_720_40.png" }, /*alpha*/ undefined, /*inputEnabled*/ false);
    const statusPosRow2 = statusPos(this.gameRefs.settings, undefined as any, undefined as any, "byOrder", undefined as any, 0, 2);
    this.bgSpritePool.newSprite(statusPosRow2.xMin - 15, statusPosRow2.yMin, {}, { sprite: "vial_720_40.png" }, /*alpha*/ undefined, /*inputEnabled*/ false);
    const statusPosRow3 = statusPos(this.gameRefs.settings, undefined as any, undefined as any, "byOrder", undefined as any, 0, 3);
    this.bgSpritePool.newSprite(statusPosRow3.xMin - 15, statusPosRow3.yMin, {}, { sprite: "vial_720_40.png" }, /*alpha*/ undefined, /*inputEnabled*/ false);
  }

  drawAnimControlBtns() {
    this.animControlBtnPool.clear();
    const types: ["pause", "play", "fast", "skip"] = ["pause", "play", "fast", "skip"];
    types.map((type, typeIndex) => {
      const pos = createPosition(this.gameRefs.settings,
        "left", 20 + 61 * typeIndex, 60,
        "top", 900, 60,
      );
      this.animControlBtnPool.newSprite(pos.xMin, pos.yMin, {}, { type });
    });
  }

  drawTreeControlBtns() {
    this.treeControlBtnPool.clear();
    const types: ["remove"] = ["remove"];
    types.map((type, typeIndex) => {
      const pos = createPosition(this.gameRefs.settings,
        "left", 50, 60,
        "top", 1000 + 50 * typeIndex, 60,
      );
      this.treeControlBtnPool.newSprite(pos.xMin, pos.yMin, {}, { type, tag: "TreeControlBtnData", });
      if (this.treeCtrl !== undefined && this.treeCtrl === type) {
        const indicator = this.treeControlBtnPool.newSprite(pos.xMin - 5, pos.yMin - 5, {}, { tag: "Indicator"});
        indicator.inputEnabled = false;
      }
    });
  }

  drawSwitchOrderBtns() {
    this.switchStatusOrderBtnPool.clear();
    const pos = createPosition(this.gameRefs.settings,
      "left", unitEnMinX - 250, 60,
      "top", unitFrMinY - 50, 60,
    );
    this.switchStatusOrderBtnPool.newSprite(pos.xMin, pos.yMin, {}, {});
  }

  drawStats(
    state: GameState,
  ) {
    this.statsTextPool.clear();
    this.detailBtnPool.clear();
    this.detailExplPool.clear();
    this.hoverSpritePool.clear();
    this.hoverGraphicsPool.clear();

    // hover bar
    state.frUnits.units.forEach((unit, unitIndex) => {
      if (unit !== undefined) {
        // hover bar
        if (
          this.hoveredUnit !== undefined && deepEqual(this.hoveredUnit, unit.id)
        ) {
          const unitPos = friendlyUnitPos(this.gameRefs.settings, state, unitIndex);
          const hoverBarPos = relativeTo(unitPos,
            [{ type: "above", amt: 5 }, { type: "left", amt: 5 }],
            0, 0,
          );
          const sprite = this.hoverSpritePool.newSprite(hoverBarPos.xMin, hoverBarPos.yMin, {}, {});
          sprite.inputEnabled = false;
        }
      }
    });
    state.enUnits.units.forEach((unit, unitIndex) => {
      if (unit !== undefined) {
        // hover bar
        if (
          this.hoveredUnit !== undefined && deepEqual(this.hoveredUnit, unit.id)
        ) {
          const unitPos = enemyUnitPos(this.gameRefs.settings, state, unitIndex);
          const hoverBarPos = relativeTo(unitPos,
            [{ type: "above", amt: 5 }, { type: "left", amt: 5 }],
            0, 0,
          );
          const sprite = this.hoverSpritePool.newSprite(hoverBarPos.xMin, hoverBarPos.yMin, {}, {});
          sprite.inputEnabled = false;
        }
      }
    });
    groupOrder.forEach((tag, tagIndex) => {
      state.statusRows[tag].statuses.forEach((status, statusIndex) => {
        const triggerPos = statusPos(this.gameRefs.settings, state, this.statusesById!, this.statusOrder, status.id, statusIndex, tagIndex);
        // hover graphics
        if (
          this.hoveredUnit !== undefined && deepEqual(this.hoveredUnit, status.id)
        ) {
          this.hoverGraphicsPool.beginFill();
          this.hoverGraphicsPool.lineStyle(4);
          const centerTriggerPos = center(triggerPos);
          this.hoverGraphicsPool.moveTo(centerTriggerPos.x, centerTriggerPos.y);
          if (status.owner.type === "friendly") {
            const ownerPosCenter = center(friendlyUnitPos(this.gameRefs.settings, state, new EntityId(status.owner.id, "friendly")));
            this.hoverGraphicsPool.lineTo(ownerPosCenter.x, ownerPosCenter.y); 
          }
          this.hoverGraphicsPool.endFill();
        }
      });
    });

    // DETAIL
    const showUnit = this.hoveredUnit;
    if (showUnit !== undefined) {
      if (showUnit.type === "status") {
        // compiler does not refine type of this id
        const statusId = showUnit as StatusId;

        const unit: StStatus | undefined = getTarget(state, statusId);
        if (unit !== undefined) {
          const pos1 = createPosition(this.gameRefs.settings,
            "left", 650, 150,
            "bot", 100, 300,
          );
          this.statsTextPool.newText(pos1, `${unit.tag}: ${unit.value} (${unit.hp})`);
        }
      }
    }

    // draw ability if currently showing
    if (this.displayedAbility !== undefined) {
      groupFromDesc(
        abilityDescription(this.displayedAbility),
        80, { x: explX, y: explY }, () => { return {} }, sprite => { return { sprite }},
        this.detailExplPool,
      );
    }
  }

  findUnit(
    globalId: UnitId | "noOrigin",
  ): DataSprite<UnitData> | undefined {
    let returnRef: DataSprite<UnitData> | undefined = undefined;
    this.unitPool.forEachAlive((ref: DataSprite<UnitData>) => {
      if (deepEqual(ref.data.globalId, globalId)) {
        returnRef = ref;
      }
    });
    return returnRef;
  }

  drawIntermediateActions(
    state: GameState,
    prevState: GameState | undefined,
    ability: Ability,
    origin: UnitId,
    log: Log,
  ): Animation {
    const maxTypeIndex = Math.max(...log.map(x => x.typeIndex));
    const split = splitLog(log);
    const anims: Animation[] = range0(maxTypeIndex + 1).map(typeIndex => {
      const createBg = new Create(() => {
        this.logTextPool.clear();
        this.logTextSpritePool.clear();
        this.logTriggerPool.clear();
        this.intermediateBgPool.clear();
        this.detailExplPool.clear();
        
        this.interactionEnabled = false;
        this.displayedAbility = undefined;

        if (split[typeIndex].length > 0) {
          this.intermediate = split[typeIndex][0].intermediateIndex;
        }

        this.drawCurrentState();

        const pos = logPosition(this.gameRefs.settings, 0, typeIndex);
        const sprite = this.intermediateBgPool.newSprite(pos.xMin, pos.yMin, {}, { sprite: "grey_border1.png"}, 0.3);
        return sprite;
      }, self => {
        if (typeIndex === 0) {
          // start turn
          const logIcon = this.drawLogIcon(0, log);
          return logIcon;
        } else if (typeIndex === 1) {
          this.sourceUnit = origin;
          // friendly action
          const friendlyUnit = this.findUnit(origin);
          if (friendlyUnit === undefined) {
            return new BaseAnimation(1, animationDummy, t => {
              t.to({ value: 0 }, 1);
            });
          }
          const preX = friendlyUnit.x;
          const preY = friendlyUnit.y;
          const moveFr = new BaseAnimation(1000, friendlyUnit, t => {
            const sourcePos = sourceUnitPos();
            t.to({ x: sourcePos.xMin, y: sourcePos.yMin }, 1000);
          });
          const showAbility = new Create(() => {
            this.displayedAbility = ability;
            return groupFromDesc(abilityDescription(ability),
              80, { x: explX, y: explY }, () => { return {} }, sprite => { return { sprite }},
              this.detailExplPool,
            );
          }, self => {
            return new BaseAnimation(1000, self, t => {
              t.from({ alpha: 0 }, 1000);
            });
          });

          const actionAnims = new SeqAnimation(split[1].map((entry, actionI) => {
            return new ParAnimation([
              this.drawAction(entry),
              this.drawLogIcon(entry.logIndex, log),
            ]);
          }));

          const fadeOut = new BaseAnimation(750, friendlyUnit, t => {
            this.sourceUnit = undefined;
            t.to({ x: preX, y: preY }, 750);
          });

          return new SeqAnimation([
            new ParAnimation([moveFr, showAbility]),
            actionAnims,
            fadeOut,
          ]);
        } else {
          const en = prevState!.enUnits.units[typeIndex - 2];
          if (en === undefined) {
            return new BaseAnimation(1, animationDummy, t => {
              t.to({ value: 0 }, 1);
            });
          }
          this.sourceUnit = en.id;
          // enemy action
          const enemyUnit = this.findUnit(en.id);
          // if 'enemyUnit' is undefined now, but 'en' was not undefined:
          // this means that this enemy died during one of the previous actions
          if (enemyUnit === undefined) {
            return new BaseAnimation(1, animationDummy, t => {
              t.to({ value: 0 }, 1);
            });
          }
          const preX = enemyUnit.x;
          const preY = enemyUnit.y;
          const moveEn = new BaseAnimation(1000, enemyUnit, t => {
            const sourcePos = sourceUnitPos();
            t.to({ x: sourcePos.xMin, y: sourcePos.yMin }, 1000);
          });
          const showAbility = new Create(() => {
            this.displayedAbility = en.abilities[aiPosToIndex(en.aiPosition)].ability;
            return groupFromDesc(abilityDescription(en.abilities[aiPosToIndex(en.aiPosition)].ability),
              80, { x: explX, y: explY }, () => { return {} }, sprite => { return { sprite }},
              this.detailExplPool,
            );
          }, self => {
            return new BaseAnimation(1000, self, t => {
              t.from({ alpha: 0 }, 1000);
            });
          });

          const actionAnims = new SeqAnimation(split[typeIndex].map((entry, actionI) => {
            return new ParAnimation([
              this.drawAction(entry),
              this.drawLogIcon(entry.logIndex, log),
            ]);
          }));

          const fadeOut = new BaseAnimation(750, enemyUnit, t => {
            this.sourceUnit = undefined;
            t.to({ x: preX, y: preY }, 750);
          });

          return new SeqAnimation([
            new ParAnimation([moveEn, showAbility]),
            actionAnims,
            fadeOut,
          ]);
        }
      });
      return createBg;
    });
    const drawAction = new BaseAnimation(1, undefined, tween => {
      this.intermediateBgPool.clear();   
      this.interactionEnabled = true;
      this.displayedAbility = undefined;
      this.intermediate = undefined;

      this.drawCurrentState();
    });
    return new SeqAnimation(anims.concat(drawAction));

    /*const anims: Animation[] = allLogIndices(log).map(x => {
      return new Create(
        () => { return; },
        () => {
          return this.drawIntermediateAction(x.logIndex);
        }
      );
    });
    const drawAction = new BaseAnimation(1, undefined, tween => {
      this.drawCurrentState();
    });
    return new SeqAnimation(anims.concat(drawAction));*/
  }

  drawAction(
    entry: LogEntry,
  ) {
    const action: ActionWithOrigin = entry.action;
    const actionI: number = entry.actionWithinAbility;
    const intermediateIndex: number = entry.intermediateIndex;
    const targets = actionTargets(action);
    if (targets.length > 0) {
      const target0 = targets[0];
      let targetRef: DataSprite<UnitData> | undefined;
      this.unitPool.forEachAlive((ref: DataSprite<UnitData>) => {
        if (deepEqual(ref.data.globalId, target0)) {
          targetRef = ref;
        }
      });
      if (targetRef === undefined) {
        throw "drawAction: target is not defined";
      }
      const preX = targetRef.x;
      const preY = targetRef.y;
      const targetAnimPre = new BaseAnimation(1000, targetRef, t => {
        t.to({ x: 850, y: 600 }, 1000);
      });
      const actionBg = new Create(() => {
        this.intermediate = intermediateIndex;
        this.drawCurrentState();
        return this.actionBgPool.newSprite(explX, explY - 20 + 80 * actionI, {}, { sprite: "grey_border2.png"}, 0.3);
      }, self => {
        return new BaseAnimation(500, self, t => {
          t.from({ alpha: 0 }, 500);
        });
      });
      const targetAnimPost = new BaseAnimation(750, targetRef, t => {
        this.actionBgPool.clear();
        t.to({ x: preX, y: preY }, 750);
      });
      const removeBg = new BaseAnimation(1, animationDummy, t => {
        this.actionBgPool.clear();
        t.to({ value: 0 }, 1);
      });
      if (deepEqual(targetRef.data.globalId, action.origin)) {
        return new SeqAnimation([
          actionBg,
          this.targetAnimMid(action, 700),
          removeBg,
        ]);
      }
      return new SeqAnimation([
        new ParAnimation([targetAnimPre, actionBg]),
        this.targetAnimMid(action, 850),
        targetAnimPost,
        removeBg,
      ]);
    } else {
      // TODO: animation without target
      return new BaseAnimation(1, undefined, tween => {
      });
    }
  }

  targetAnimMid(
    action: Action,
    x: number,
  ): Animation {
    return new Create(() => {
      return this.createLogTextSprite(x, 600, action);
    }, self => {
      return new BaseAnimation(1000, self, t => {
        t.from({ y: self.y + 200 }, 1000);
        t.onComplete.add(() => {
          self.destroy();
          this.logGraphicsPool.clear();
        });
      });
    });
  }

  drawLogIcon(
    intermediate: LogIndex,
    log: Log,
  ): Animation {

    const state = this.currentState();
    const logEntry = getLogEntry(log, intermediate);
    const prevLog = getPrevLogEntry(log, intermediate);

    // draw log icons
    const anims: Animation[] = allLogIndices(log).map(x => {
      const entryIndex = x.entryIndex;
      const typeIndex = x.typeIndex;
      const entry = getLogEntry(log, x.logIndex);

      const pos = createPosition(this.gameRefs.settings,
        "left", 20 + 50 * entryIndex, 40,
        "top", 120 + 80 * typeIndex, 40,
      );

      if (logIndexLt(x.logIndex, intermediate)) {
        return new Create(() => {
          let sprite: Phaser.Sprite;
          sprite = this.logActionPool.newSprite(pos.xMin, pos.yMin, {}, {...entry, ...{ logIndex: x.logIndex }});
          sprite.alpha = 1;
          return sprite;
        }, self => {
          return new BaseAnimation(0, self, t => { return; } );
        });
      } else if (logIndexEq(x.logIndex, intermediate)) {
        return new Create(() => {
          let sprite: Phaser.Sprite;
          sprite = this.logActionPool.newSprite(pos.xMin, pos.yMin, {}, {...entry, ...{ logIndex: x.logIndex }});
          sprite.alpha = 0.5;
          return sprite;
        }, self => {
          return new BaseAnimation(500, self, t => { t.to({ alpha: 1.0 }, 500); } );
        });
      } else {
        return new Create(() => {
          let sprite: Phaser.Sprite;
          sprite = this.logActionPool.newSprite(pos.xMin, pos.yMin, {}, {...entry, ...{ logIndex: x.logIndex }});
          sprite.alpha = 0.5;
          return sprite;
        }, self => {
          return new BaseAnimation(0, self, t => { return; } );
        });
      }
    });

    // draw difference with prev log
    if (prevLog !== undefined) {
      // console.log(`PREV: ${JSON.stringify(prevLog.action)}`)
    }

    return new Create(() => {
      this.logActionPool.clear();
      this.logTriggerPool.clear();
    }, self => {
      return new ParAnimation(anims);
    });
  }

  drawUnitSelect(
  ) {
    this.unitSelectPool.clear();
    this.unitSelectBgPool.clear();

    if (this.selecting !== undefined) {
      const bgPos = relativeTo(friendlyUnitPos(this.gameRefs.settings, this.currentState(), this.selecting),
        [{type: "above", amt: 10}], 550, 190
      );
      this.unitSelectBgPool.newSprite(bgPos.xMin, bgPos.yMin, {}, {});

      const levelId = selectedLevelId(this.gameRefs);
      const slots = levelData[levelId!].slots;
      const cardIds = levelData[levelId!].cardIds[(slots - 1) - this.selecting];
      cardIds.forEach((cardId, cardIdIndex) => {
        this.unitSelectPool.newSprite(bgPos.xMin + 20 + 170 * cardIdIndex, bgPos.yMin + 20, {}, { cardId });
      });
    }
  }

  createEffectAnimation(
    state: GameState,
    lastAction: Action,
    transforms: StatusLog[],
  ): Animation {
    return this._createEffectAnimation(state, lastAction, transforms, 0, []);
  }

  _createEffectAnimation(
    state: GameState,
    lastAction: Action,
    transforms: StatusLog[],
    index: number,
    sprites: Phaser.Sprite[],
  ): Animation {
    let action: Action = undefined as any;
    if (index < transforms.length) {
      action = transforms[index].before;
    } else {
      action = lastAction;
    }

    return new Create(() => {
      // draw connection line
      this.logGraphicsPool.clear();
      const logLoc = this.logLocation(action, state);
      if (logLoc !== undefined) {
        drawLine(this.logGraphicsPool, logLoc, explArrowEnd);
      }
      // return sprite
      return this.createLogTextSprite(explX, explY - 160 + (80 * index), action);
    }, self => {
      if (index < transforms.length) {
        return new SeqAnimation([
          new BaseAnimation(1000, self, t => {
            t.from({ alpha: 0.3 }, 1000);
          }),
          this._createEffectAnimation(state, lastAction, transforms, index + 1, sprites.concat(self)),
        ]);
      } else {
        return new BaseAnimation(1000, self, t => {
          t.from({ alpha: 0.3 }, 1000);
          t.onComplete.add(() => {
            self.destroy();
            sprites.forEach(x => x.destroy())
            this.logGraphicsPool.clear();
          });
        });
      }
    });
  }

  createEffectAnimationWithZoom(
    state: GameState,
    lastAction: ActionWithOrigin,
    transforms: StatusLog[],
  ): Animation {
    const origin = lastAction.origin;

    let originAnimPre: Animation = new BaseAnimation(0, self, t => { return; } );
    let originAnimPost: Animation = new BaseAnimation(0, self, t => { return; } );
    if (origin !== "noOrigin") {
      let originRef: DataSprite<UnitData> | undefined = undefined;
      this.unitPool.forEachAlive((ref: DataSprite<UnitData>) => {
        if (deepEqual(ref.data.globalId, origin)) {
          originRef = ref;
        }
      });
      if (originRef !== undefined) {
        originAnimPre = new BaseAnimation(1000, originRef, t => {
          t.to({ x: 800, y: 600 }, 1000);
        });
        originAnimPost = new BaseAnimation(150, originRef, t => {
            t.to({ alpha: 0.5 }, 150);
        });
      }
    }

    let targetAnimPre: Animation = new BaseAnimation(0, self, t => { return; } );
    let targetAnimMid: Animation = new BaseAnimation(0, self, t => { return; } );
    let targetAnimPost: Animation = new BaseAnimation(0, self, t => { return; } );
    const targets = actionTargets(lastAction);
    if (targets.length > 0) {
      const target0 = targets[0];
      let targetRef: DataSprite<UnitData> | undefined = undefined;
      this.unitPool.forEachAlive((ref: DataSprite<UnitData>) => {
        if (deepEqual(ref.data.globalId, target0)) {
          targetRef = ref;
        }
      });
      if (targetRef !== undefined) {
        targetAnimPre = new BaseAnimation(1000, targetRef, t => {
          t.to({ x: 1000, y: 600 }, 1000);
        })
        targetAnimMid = new Create(() => {
          return this.createLogTextSprite(1000, 600, lastAction);
        }, self => {
          return new BaseAnimation(1000, self, t => {
            t.from({ y: self.y + 200 }, 1000);
            t.onComplete.add(() => {
              self.destroy();
              this.logGraphicsPool.clear();
            });
          });
        }),
        targetAnimPost = new BaseAnimation(150, targetRef, t => {
          t.to({ alpha: 0.5 }, 150);
        })
      }
    }

    return new SeqAnimation([
      new ParAnimation([originAnimPre, targetAnimPre]),
      targetAnimMid,
      // new ParAnimation([originAnimPost, targetAnimPost]),
      // this._createEffectAnimationWithZoom(state, lastAction, transforms, index + 1),
    ]);

    //return this._createEffectAnimationWithZoom(state, lastAction, transforms, 0);
  }

  _createEffectAnimationWithZoom(
    state: GameState,
    lastAction: ActionWithOrigin,
    transforms: StatusLog[],
    index: number,
  ): Animation {
    return undefined as any;
  }

  logLocation(
    action: Action,
    state: GameState,
  ): { x: number, y: number } | undefined {
    switch (action.tag) {
      case "AddStatus": // fallthrough
      case "UseCharge": // fallthrough
      case "RestoreCharge": // fallthrough
      case "MoveAI": // fallthrough
      case "Heal": // fallthrough
      case "Damage": {
        return center(this.onTargetPos(state, action.target));
      }
      case "RemoveThreat": // fallthrough
      case "AddThreat": {
        return center(this.onTargetPos(state, action.forAlly));
      }
      case "Death": // fallthrough TODO: this should point to the location of where the status was?
      case "Combined": // fallthrough
      case "Invalid": // fallthrough
      case "StartTurn": {
        return undefined;
      }
    }
  }

  onTargetPos(
    state: GameState,
    _id: TargetId,
  ) {
    switch (_id.type) {
      case "status": {
        const id = _id as StatusId;

        return statusPos(this.gameRefs.settings, state, this.statusesById!, this.statusOrder, id);
      }
      case "enemy": {
        const id = _id as EnemyId;

        return enemyUnitPos(this.gameRefs.settings, state, id);
      }
      case "friendly": {
        const id = _id as FriendlyId;

        return friendlyUnitPos(this.gameRefs.settings, state, id);
      }
    }
  }

  createLogTextSprite(
    parentX: number,
    parentY: number,
    action: Action,
  ): Phaser.Sprite {
    return groupFromDesc(
      actionDescription(action),
      80, { x: parentX, y: parentY }, () => { return {} }, sprite => { return { sprite }},
      this.logTextSpritePool,
    );
  }

  createTriggerEntryAnim(
    statusLog: StatusLog,
    pos: Position,
  ) {
    return {
      create: () => {
        const sprite = this.logTriggerPool.newSprite(pos.xMin, pos.yMin, {}, statusLog);
        return sprite;
      },
      introTween: (sprite: DataSprite<LogTriggerData>) => {
        const tween = this.logTriggerPool.introTween(sprite);
        if (tween !== undefined) {
          const textPos = createPosition(this.gameRefs.settings,
            "left", 680, 100,
            "top", 200, 100,
          );
          addTextPopup(
            this.gameRefs,
            tween.first,
            () => {
              return this.logTextPool.newText(textPos, `${statusLog.tag}`);
            },
            tween => {
              tween.to({ y: textPos.yMin - 100 }, 1000);
            },
            "log",
          );
        }
        return tween;
      },
    };
  }

  drawFrames(
    state: GameState,
    action: Action,
    origin: UnitId | undefined,
  ): Animation {
    let targetFrames: Animation;
    switch (action.tag) {
      case "Damage": // fallthrough
      case "AddStatus": // fallthrough
      case "UseCharge": {
        targetFrames = this.createFrame(state, action.target, "in");
        break;
      }
      case "AddThreat": {
        targetFrames = this.createFrame(state, action.atEnemy, "in");
        break;
      }
      default: {
        targetFrames = new SeqAnimation([]);
        break;
      }
    }

    const originFrames: Animation = origin === undefined
      ? new SeqAnimation([])
      : this.createFrame(state, origin, "out");

    return new ParAnimation([
      targetFrames,
      originFrames,
    ]);
  }

  createFrame(
    state: GameState,
    id: TargetId,
    type: "out" | "in",
  ): Create {
    const pos = this.onTargetPos(state, id);
    const sprite = id.type === "status"
      ? "icon_add_status.png"
      : `frame_${type}.png`;
    return new Create(
      () => {
        return this.framePool.newSprite(pos.xMin - 5, pos.yMin - 5, {}, { sprite });
      },
      self => {
        return new BaseAnimation(999, self, t => {
          t.to({ x: pos.xMin - 5 }, 125);
          t.to({ x: pos.xMin + 5 }, 250);
          t.to({ x: pos.xMin - 5 }, 250);
          t.to({ x: pos.xMin + 5 }, 250);
          t.to({ x: pos.xMin }, 125);
          t.onComplete.add(() => self.kill());
        });
      }
    );
  }

  drawTree(
    solInfo: {
      solution: Solution,
      loc: Location,
    }
  ) {
    this.solTreePool.clear();

    const x = 100;
    const y = 950;

    const initialNodeType = solInfo.loc.length === 0 ? "node_full" : "node";
    const sprite = this.solTreePool.newSprite(x - 50, y, {}, { type: initialNodeType, loc: [] });

    const drawPosList = drawPositions(solInfo.solution.tree);
      drawPosList.forEach((drawPos) => {
        const type = JSON.stringify(drawPos.loc) === JSON.stringify(solInfo.loc) ? "node_full" : "node";
        const sprite = this.solTreePool.newSprite(x + drawPos.x * 50, y + drawPos.y * 50, {}, { type, loc: drawPos.loc });
    });
  }

  clearTree() {
    this.solTreePool.clear();
  }

  setLogAnimationSpeed(
    speed: SpeedType,
  ) {
    this.gameRefs.saveData.animationSpeeds.log = speed;
    const tweens = this.gameRefs.game.tweens.getAll();
    tweens.forEach(tween => {
      if ( (<any>tween).data !== undefined
        && (<any>tween).data.log !== undefined
        && (<any>tween).data.log === true
      ) {
        tween.timeScale = speedTypeToSpeed(speed);
      }
    });
  }

  setTreeControl(
    control: "remove" | undefined
  ) {
    this.treeCtrl = control;
    this.drawTreeControlBtns();
  }

  setVisibility(
    visibility: boolean,
  ) {
    this.clearBtnPool.visible = visibility;
    this.unitPool.visible = visibility;
    this.unitResPool.visible = visibility;
    this.abilityPool.visible = visibility;
    this.triggerPool.visible = visibility;
    this.logActionPool.visible = visibility;
    this.logTriggerPool.visible = visibility;
    this.statsTextPool.setVisiblity(visibility);
    this.unitTextPool.setVisiblity(visibility);
    this.logTextPool.setVisiblity(visibility);
    this.animControlBtnPool.visible = visibility;
    this.solTreePool.visible = visibility;
    this.detailBtnPool.visible = visibility;
  }
}

function mkClearBtnPool(
  gameRefs: GameRefs,
): Pool<{}, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return "icon_clear_100_100.png";
      },
      introAnim: [
      ],
      callbacks: {
        click: (self) => {
          // clearSolution(gameRefs);
        },
        hoverOver: (self) => {
          addShader(gameRefs, self, "blue-glow");
        },
        hoverOut: (self) => {
          clearShader(self);
        },
      },
    },
  );
}

type UnitData = {
  cardId: CardId,
  globalId: EntityId<"friendly" | "enemy">,
  position: number,
};

function mkUnitPool(
  gameRefs: GameRefs,
): Pool<UnitData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return cardMap[self.data.cardId];
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 20, Phaser.Easing.Linear.None, false, 5);
        },
      ],
      callbacks: {
        click: (self) => {
          if (gameRefs.screens.execScreen.interactionEnabled) {
            const clickState = gameRefs.screens.execScreen.clickState;
            if (clickState !== undefined) {
              const inputType: UserInput = clickState.ability.inputs[clickState.inputs.length];
              if (matchUserInput(inputType, self.data.globalId)) {
                clickState.inputs.push(self.data.globalId);
                if (clickState.inputs.length === clickState.ability.inputs.length) {
                  extendLevelSolution(gameRefs, gameRefs.screens.execScreen.solDataFromClickState());
                }
              }
            } else {
              if (
                gameRefs.screens.execScreen.selecting === undefined ||
                gameRefs.screens.execScreen.selecting !== self.data.position
              ) {
                showChangeUnitScreen(gameRefs, self.data.position);
              } else {
                hideChangeUnitScreen(gameRefs);
              }
            }
          }
        },
        hoverOver: (self) => {
          hoverUnit(gameRefs, self.data.globalId);
        },
        hoverOut: (self) => {
          clearHover(gameRefs);
        },
      },
    },
  );
}

type EmptySlotData = {
  position: number,
};

function mkEmptySlotPool(
  gameRefs: GameRefs,
): Pool<EmptySlotData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return "empty_slot_150_150.png";
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 20, Phaser.Easing.Linear.None, false, 5);
        },
      ],
      callbacks: {
        click: (self) => {
          if (
            gameRefs.screens.execScreen.selecting === undefined ||
            gameRefs.screens.execScreen.selecting !== self.data.position
          ) {
            showChangeUnitScreen(gameRefs, self.data.position);
          } else {
            hideChangeUnitScreen(gameRefs);
          }
        },
      },
    },
  );
}

type UnitResData = {
  cardId: string,
  globalId: EntityId<"friendly" | "enemy">,
  type: "hp" | "ch" | "th" | "res_anim",
};

function mkUnitResPool(
  gameRefs: GameRefs,
): Pool<UnitResData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        if (self.data.type === "th") {
          return "ch.png";
        }
        return `${self.data.type}.png`;
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 20, Phaser.Easing.Linear.None, false, 5);
        },
      ],
      callbacks: {
        click: (self) => {
          const clickState = gameRefs.screens.execScreen.clickState;
          if (clickState !== undefined) {
            const inputType: UserInput = clickState.ability.inputs[clickState.inputs.length];
            if (matchUserInput(inputType, self.data.globalId)) {
              clickState.inputs.push(self.data.globalId);
              if (clickState.inputs.length === clickState.ability.inputs.length) {
                extendLevelSolution(gameRefs, gameRefs.screens.execScreen.solDataFromClickState());
              } else {
                gameRefs.screens.execScreen.drawCurrentState();
              }
            }
          }
        },
        hoverOver: (self) => {
          hoverUnit(gameRefs, self.data.globalId);
        },
        hoverOut: (self) => {
          clearHover(gameRefs);
        },
      },
    },
  );
}

type FrAbilityData = {
  ability: FrAbility,
  spriteId: string,
  index: number,
  globalId: EntityId<"friendly" | "enemy">,
  tag: "FrAbilityData",
}

type EnAbilityData = {
  ai: EnAbility,
  aiIndex: number,
  tag: "EnAbilityData",
}

type IndicatorData = {
  tag: "Indicator"
}

type AbilityData = FrAbilityData | EnAbilityData | IndicatorData;

function mkAbilityPool(
  gameRefs: GameRefs,
): Pool<AbilityData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        switch (self.data.tag) {
          case "EnAbilityData": return `${self.data.ai.spriteId}.png`;
          case "FrAbilityData": return `${self.data.spriteId}.png`;
          case "Indicator" : return "icon_add_status.png";
        }
        
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 20, Phaser.Easing.Linear.None, false, 5);
        },
      ],
      callbacks: {
        click: (self) => {
          if (self.data.tag === "FrAbilityData") {
            if (
              gameRefs.screens.execScreen.canExtendState() &&
              gameRefs.screens.execScreen.clickState === undefined &&
              gameRefs.screens.execScreen.interactionEnabled
            ) {
              // start using ability, if applicable
              gameRefs.screens.execScreen.clickState = {
                ability: self.data.ability,
                inputs: [],
                origin: self.data.globalId,
                index: self.data.index,
              }
              if (self.data.ability.inputs.length === 0) {
                extendLevelSolution(gameRefs, gameRefs.screens.execScreen.solDataFromClickState());
              } else {
                gameRefs.screens.execScreen.drawCurrentState();
              }
            } else if (
              gameRefs.screens.execScreen.clickState !== undefined &&
              deepEqual(gameRefs.screens.execScreen.clickState.origin, self.data.globalId) &&
              gameRefs.screens.execScreen.clickState.index === self.data.index
            ) {
              // cancel using ability, if applicable
              gameRefs.screens.execScreen.clickState = undefined;
              gameRefs.screens.execScreen.drawCurrentState();
            }
          }
        },
        hoverOver: (self) => {
          if (gameRefs.screens.execScreen.interactionEnabled) {
            if (! (self.data.tag === "FrAbilityData" &&
              gameRefs.screens.execScreen.clickState !== undefined &&
              deepEqual(gameRefs.screens.execScreen.clickState.origin, self.data.globalId) &&
              self.data.index === gameRefs.screens.execScreen.clickState.index
            )) {
              addShader(gameRefs, self, "blue-glow");
            }
            if (self.data.tag === "FrAbilityData" || self.data.tag === "EnAbilityData") {
              let ability: Ability;
              if (self.data.tag === "FrAbilityData") {
                ability = self.data.ability.ability;
              } else {
                ability = self.data.ai.ability;
              }
              
              return groupFromDesc(
                abilityDescription(ability),
                80, { x: explX, y: explY }, () => { return {} }, sprite => { return { sprite }},
                gameRefs.screens.execScreen.detailExplPool,
              );
            }
          }
          return undefined as any;
        },
        hoverOut: (self) => {
          if (gameRefs.screens.execScreen.interactionEnabled) {
            if (! (self.data.tag === "FrAbilityData" &&
              gameRefs.screens.execScreen.clickState !== undefined &&
              deepEqual(gameRefs.screens.execScreen.clickState.origin, self.data.globalId) &&
              self.data.index === gameRefs.screens.execScreen.clickState.index
            )) {
              clearShader(self);
            }
            gameRefs.screens.execScreen.detailExplPool.clear();
          }
        },
      },
    },
  );
}

type TriggerData = {
  status: StStatus,
};

function mkTriggerPool(
  gameRefs: GameRefs,
): Pool<TriggerData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        switch (self.data.status.tag) {
          case "Armor": return "icon_armor_40_40.png";
          case "Weak": return "icon_weak_40_40.png";
          case "Strong": return "icon_strong_40_40.png";
          case "Fragile": return "icon_fragile_40_40.png";
          case "OnDeath": return "icon_death_40_40.png";
          default: return "icon_a.png";
        }
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 20, Phaser.Easing.Linear.None, false, 5);
        },
      ],
      callbacks: {
        click: (self) => {
          const globalId = self.data.status.id;
          const clickState = gameRefs.screens.execScreen.clickState;
          if (clickState !== undefined) {
            const inputType: UserInput = clickState.ability.inputs[clickState.inputs.length];
            if (matchUserInput(inputType, globalId)) {
              clickState.inputs.push(globalId);
              if (clickState.inputs.length === clickState.ability.inputs.length) {
                extendLevelSolution(gameRefs, gameRefs.screens.execScreen.solDataFromClickState());
              }
            }
          }
        },
        hoverOver: (self) => {
          hoverUnit(gameRefs, self.data.status.id);
        },
        hoverOut: (self) => {
          clearHover(gameRefs);
        },
      },
    },
  );
}

type LogActionData = LogEntryI;

function mkLogActionPool(
  gameRefs: GameRefs,
): Pool<LogActionData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        switch (self.data.action.tag) {
          case "Damage": return "icon_damage_40_40.png";
          case "Heal": return "icon_heal_40_40.png";
          case "UseCharge": return "icon_usecharge_40_40.png";
          case "RestoreCharge": return "icon_restorecharge_40_40.png";
          case "AddThreat": return "icon_addthreat_40_40.png";
          case "RemoveThreat": return "icon_removethreat_40_40.png";
          case "MoveAI": return "icon_ai.png";
          case "StartTurn": return "icon_start_turn_40_40.png";
          case "AddStatus": return "icon_addstatus_40_40.png";
          case "Death": return "icon_death_40_40.png";
          case "Invalid": return "icon_invalid_40_40.png";
          case "Combined": return "icon_b.png";
        }
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 1000, Phaser.Easing.Linear.None, false, 5);
          (<any>tween).data = { log: true };
          tween.timeScale = speedTypeToSpeed(gameRefs.saveData.animationSpeeds.log);
        },
      ],
      callbacks: {
        click: (self) => {
          // clear animations
          clearAnimations(gameRefs.game, gameRefs.screens.execScreen);
          gameRefs.screens.execScreen.clearAnimPools();

          runAsTween(gameRefs, gameRefs.screens.execScreen.drawIntermediateAction(self.data.logIndex), "log");
        },
        hoverOver: (self) => {
          // change state/stats
          gameRefs.screens.execScreen.drawState(self.data.state);
          gameRefs.screens.execScreen.drawStats(self.data.state);

          // change hover info
          self.data.transforms.forEach((transform, transformIndex) => {
            groupFromDesc(
              actionDescription(transform.before),
              80, { x: 50, y: 50 * transformIndex}, () => { return {} }, sprite => { return { sprite }},
              gameRefs.screens.execScreen.detailExplPool,
            );
            if (transformIndex === self.data.transforms.length - 1) {
              groupFromDesc(
                actionDescription(transform.after),
                80, { x: 50, y: 50 * (transformIndex + 1)}, () => { return {} }, sprite => { return { sprite }},
                gameRefs.screens.execScreen.detailExplPool,
              );
            }
            groupFromDesc(
              statusTagDescription(transform.tag),
              80, { x: 0, y: 50 * (transformIndex + 1)}, () => { return {} }, sprite => { return { sprite }},
              gameRefs.screens.execScreen.detailExplPool,
            );
          });
        },
        hoverOut: (self) => {
          gameRefs.screens.execScreen.drawCurrentState();
        },
      },
    },
  );
}

type LogTriggerData = {
  tag: StatusTag,
  before: Action,
  after: Action,
};

function mkLogTriggerPool(
  gameRefs: GameRefs,
): Pool<LogTriggerData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return `icon_c.png`;
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 1000, Phaser.Easing.Linear.None, false, 5);
          (<any>tween).data = { log: true };
          tween.timeScale = speedTypeToSpeed(gameRefs.saveData.animationSpeeds.log);
        },
      ],
      callbacks: {
        click: (self) => {
        },
        hoverOver: (self) => {
          //gameRefs.screens.execScreen.drawState(self.data.state);
          //gameRefs.screens.execScreen.drawStats(self.data.state);
        },
        hoverOut: (self) => {
          //gameRefs.screens.execScreen.drawState(gameRefs.screens.execScreen.state!);
          //gameRefs.screens.execScreen.drawStats(gameRefs.screens.execScreen.state!);
        },
      },
    },
  );
}

type AnimControlBtn = {
  type: "pause" | "play" | "fast" | "skip",
};

function mkAnimControlBtnPool(
  gameRefs: GameRefs,
): Pool<AnimControlBtn, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        switch (self.data.type) {
          case "pause": return "icon_anim_pause.png";
          case "play": return "icon_anim_play.png";
          case "fast": return "icon_anim_ff.png";
          case "skip": return "icon_anim_skip.png";
        }
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 1000, Phaser.Easing.Linear.None, false, 5);
        },
      ],
      callbacks: {
        click: (self) => {
          gameRefs.screens.execScreen.setLogAnimationSpeed(self.data.type);
        },
      },
    },
  );
}

type SolTreeData = {
  type: "node" | "branch" | "node_full",
  loc: Location,
};

function mkSolTreePool(
  gameRefs: GameRefs,
): Pool<SolTreeData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        switch (self.data.type) {
          case "branch": return "tree_branch.png";
          case "node": return "tree_node.png";
          case "node_full": return "tree_node_full.png";
        }
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 1000, Phaser.Easing.Linear.None, false, 5);
        },
      ],
      callbacks: {
        click: (self) => {
          if (gameRefs.screens.execScreen.treeCtrl === undefined) {
            changeLevelLoc(gameRefs, self.data.loc);
          } else if (gameRefs.screens.execScreen.treeCtrl === "remove") {
            cutLevelSolution(gameRefs, self.data.loc);
            gameRefs.screens.execScreen.setTreeControl(undefined);
          }
        },
      },
    },
  );
}

type DetailBtnData = {
  type: CodexTypes
};

function mkDetailBtnPool(
  gameRefs: GameRefs,
): Pool<DetailBtnData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return "icon_a.png"
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 1000, Phaser.Easing.Linear.None, false, 5);
        },
      ],
      callbacks: {
        click: (self) => {
          transitionScreen(gameRefs, new ScreenCodex(self.data.type));
        },
      },
    },
  );
}

type DetailExplData = {
  sprite: string,
};

function mkDetailExplPool(
  gameRefs: GameRefs,
): Pool<DetailExplData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return self.data.sprite;
      },
      introAnim: [
      ],
      callbacks: {
      },
    },
  );
}

type LogTextSpriteData = {
  sprite: string,
};

function mkLogTextSpritePool(
  gameRefs: GameRefs,
): Pool<LogTextSpriteData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return self.data.sprite;
      },
      introAnim: [
      ],
      callbacks: {
      },
    },
  );
}

type HoverSpriteData = {

};

function mkHoverSpritePool(
  gameRefs: GameRefs,
): Pool<HoverSpriteData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return "bar_hover.png";
      },
      introAnim: [
      ],
      callbacks: {
      },
    },
  );
}

type StateIconData = {
  sprite: string,
};

function mkStateIconPool(
  gameRefs: GameRefs,
): Pool<StateIconData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return self.data.sprite;
      },
      introAnim: [
      ],
      callbacks: {
      },
    },
  );
}

type FrameData = {
  sprite: string,
};

function mkFramePool(
  gameRefs: GameRefs,
): Pool<FrameData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return self.data.sprite;
      },
      introAnim: [
      ],
      callbacks: {
      },
    },
  );
}

type TreeControlBtnData = {
  type: "remove",
  tag: "TreeControlBtnData",
};

type TreeControlBtn = TreeControlBtnData | IndicatorData;

function mkTreeControlBtnPool(
  gameRefs: GameRefs,
): Pool<TreeControlBtn, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        if (self.data.tag === "Indicator") {
          return "icon_add_status.png";
        } else {
          switch (self.data.type) {
            case "remove": return "icon_deletetree_40_40.png";
          }
          throw "mkTreeControlBtnPool: impossible";
        }
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 1000, Phaser.Easing.Linear.None, false, 5);
        },
      ],
      callbacks: {
        click: (self) => {
          switch (gameRefs.screens.execScreen.treeCtrl) {
            case undefined: {
              gameRefs.screens.execScreen.setTreeControl("remove");
              break;
            }
            case "remove": {
              gameRefs.screens.execScreen.setTreeControl(undefined);
              break;
            }
          }
        },
      },
    },
  );
}

type SwitchStatusOrderBtnData = {
};

function mkSwitchStatusOrderBtnPool(
  gameRefs: GameRefs,
): Pool<SwitchStatusOrderBtnData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return "icon_swapstatusmode_40_40.png";
      },
      introAnim: [
      ],
      callbacks: {
        click: (self) => {
          switch (gameRefs.screens.execScreen.statusOrder) {
            case "byId": {
              gameRefs.screens.execScreen.statusOrder = "byOrder";
              gameRefs.screens.execScreen.drawCurrentState();
              break;
            }
            case "byOrder": {
              gameRefs.screens.execScreen.statusOrder = "byId";
              gameRefs.screens.execScreen.drawCurrentState();
              break;
            }
          }
        },
      },
    },
  );
}

type UnitSelectData = {
  cardId: FrUnitId,
};

function mkUnitSelectPool(
  gameRefs: GameRefs,
): Pool<UnitSelectData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return cardMap[self.data.cardId];
      },
      introAnim: [
      ],
      callbacks: {
        click: (self) => {
          deployUnit(gameRefs, self.data.cardId, gameRefs.screens.execScreen.selecting!);
        },
      },
    },
  );
}

type UnitSelectBgData = {
};

function mkUnitSelectBgPool(
  gameRefs: GameRefs,
): Pool<UnitSelectBgData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return "unit_select_bg.png";
      },
      introAnim: [
      ],
      callbacks: {
        click: (self) => {

        },
      },
    },
  );
}

type BgSpriteData = {
  sprite: string,
};

function mkBgSpritePool(
  gameRefs: GameRefs,
): Pool<BgSpriteData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return self.data.sprite;
      },
      introAnim: [
      ],
      callbacks: {
        click: (self) => {

        },
      },
    },
  );
}

type IntermediateBgData = {
  sprite: string,
};

function mkIntermediateBgPool(
  gameRefs: GameRefs,
): Pool<IntermediateBgData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return self.data.sprite;
      },
      introAnim: [
      ],
      callbacks: {
        click: (self) => {

        },
      },
    },
  );
}

type ActionBgData = {
  sprite: string,
};

function mkActionBgDataPool(
  gameRefs: GameRefs,
): Pool<ActionBgData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return self.data.sprite;
      },
      introAnim: [
      ],
      callbacks: {
        click: (self) => {

        },
      },
    },
  );
}