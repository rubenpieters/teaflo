import { Pool, mkButtonPool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { createPosition, relativeTo, Position, center } from "../../util/position";
import { addText, DataSprite } from "../../phaser/datasprite";
import { enFiltered, frFiltered, getTarget } from "../../../shared/game/state";
import { Log, LogEntry, getLogEntry, LogIndex, allLogIndices, logIndexLt, logIndexEq, getPrevLogEntry, LogEntryI, nextLogIndex, StatusLog } from "../../../shared/game/log";
import { cardMap } from "../../../app/data/cardMap";
import { TextPool } from "../../phaser/textpool";
import { EntityId, UnitId, friendlyId, TargetId, EnemyId, FriendlyId, StatusId } from "../../../shared/definitions/entityId";
import { hoverUnit, clearHover, clickUnit, extendLevelSolution, changeLevelLoc, clearSolution, cutLevelSolution } from "./events";
import { chainSpriteCreation, createTween, addTextPopup, speedTypeToSpeed, SpeedType, addSpritePopup, Create, BaseAnimation, SeqAnimation, Animation, ParAnimation, runAsTween } from "../../../app/phaser/animation";
import { drawPositions, Location } from "../../../shared/tree";
import { Solution, SolutionData } from "../../../shared/game/solution";
import { intentDescription, actionDescription, triggerTagDescription, DescToken } from "../../util/intentDesc";
import { transitionScreen, ScreenCodex } from "../transition";
import { CodexTypes } from "../codex/screen";
import { clearAnimations } from "../util";
import { friendlyUnitPos, enemyUnitPos, statusPos, unitUtilityPositions } from "./positions";
import deepEqual from "deep-equal";
import { groupOrder } from "../../../shared/game/status";
import { StStatus } from "../../../shared/definitions/statusRow";
import { matchUserInput } from "../../../shared/game/input";
import { aiIndices, indexToAiPos, aiPosToIndex } from "../../../shared/game/ai";
import { GameState } from "../../../shared/definitions/state";
import { FrAbility, EnAbility } from "../../../shared/definitions/unit";
import { Action } from "../../../shared/definitions/action";
import { UserInput } from "../../../shared/definitions/input";
import { Ability } from "../../../shared/definitions/ability";
import { StatusTag } from "../../../shared/definitions/status";

export class ExecScreen {
  clearBtnPool: Pool<{}, "neutral" | "hover" | "down">
  unitPool: Pool<UnitData, {}>
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

  animControlBtnPool: Pool<AnimControlBtn, {}>
  treeControlBtnPool: Pool<TreeControlBtn, {}>

  state: GameState | undefined
  log: Log | undefined
  hoveredUnit: TargetId | undefined
  selectedUnit: TargetId | undefined
  clickState: { ability: FrAbility, inputs: any[], origin: UnitId, index: number } | undefined
  intermediate: LogIndex | undefined

  treeCtrl: "remove" | undefined

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.clearBtnPool = mkClearBtnPool(gameRefs);
    this.unitPool = mkUnitPool(gameRefs);
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
  }

  reset() {
    this.state = undefined;
    this.log = undefined;
    this.hoveredUnit = undefined;
    this.selectedUnit = undefined;
    this.clickState = undefined;
    this.intermediate = undefined;
    this.treeCtrl = undefined;
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
    this.redrawClearBtn();
    this.clearBtnPool.playIntroAnimations();
  }

  redrawClearBtn(
  ) {
    this.clearBtnPool.clear();

    const pos = createPosition(
      "right", 100, 400,
      "bot", 500, 200,
    );
    const sprite = this.clearBtnPool.newSprite(pos.xMin, pos.yMin, "neutral", {});
    addText(this.gameRefs, sprite, pos, "Clear", "#000000", 40);
  }

  // DRAW STATE

  drawState(
    state: GameState,
    prevState?: GameState,
  ) {
    this.unitPool.clear();
    this.unitResPool.clear();
    this.triggerPool.clear();
    this.unitTextPool.clear();
    this.hoverSpritePool.clear();
    this.hoverGraphicsPool.clear();
    this.stateTextPool.clear();
    this.abilityPool.clear();
    this.stateIconPool.clear();

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
      if (unit !== undefined) {
        const unitPos = friendlyUnitPos(state, unitIndex);
        const utilityPos = unitUtilityPositions(unitPos);
        const unitSprite = this.unitPool.newSprite(unitPos.xMin, unitPos.yMin, {},
          { cardId: unit.cardId,
            globalId: unit.id,
          }
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
          (this.selectedUnit !== undefined && deepEqual(this.selectedUnit, unit.id)) ||
          (this.hoveredUnit !== undefined && deepEqual(this.hoveredUnit, unit.id))
        ) {
          const hoverBarPos = relativeTo(unitPos,
            [{ type: "above", amt: 5 }, { type: "left", amt: 5 }],
            0, 0,
          );
          const sprite = this.hoverSpritePool.newSprite(hoverBarPos.xMin, hoverBarPos.yMin, {}, {});
          sprite.inputEnabled = false;
        }

        // HP
        const unitHpPos = relativeTo(unitPos,
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
        const unitChPos = relativeTo(unitPos,
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
          const unitThPos = createPosition(
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
          const thTextPos = createPosition(
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
          const abPos = createPosition(
            "left", 250 + 170 * unitIndex + 75 * flagLeft, 70,
            "top", 830 + 75 * flagTop, 70,
          );
          this.abilityPool.newSprite(abPos.xMin, abPos.yMin, {},
            { tag: "FrAbilityData", spriteId: ability.spriteId, ability, index: abilityIndex,
              globalId: unit.id
            }
          );
          if (this.clickState !== undefined &&
            deepEqual(this.clickState.origin, unit.id) &&
            abilityIndex === this.clickState.index
          ) {
            const indicator = this.abilityPool.newSprite(abPos.xMin + 5, abPos.yMin + 5, {}, { tag: "Indicator"});
            indicator.inputEnabled = false;
          }
        });
      }
    });

    state.enUnits.units.forEach((unit, unitIndex) => {
      if (unit !== undefined) {
        const unitPos = enemyUnitPos(state, unitIndex);
        const utilityPos = unitUtilityPositions(unitPos);
        const unitSprite = this.unitPool.newSprite(unitPos.xMin, unitPos.yMin, {},
          { cardId: unit.cardId,
            globalId: unit.id,
          }
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
          (this.selectedUnit !== undefined && deepEqual(this.selectedUnit, unit.id)) ||
          (this.hoveredUnit !== undefined && deepEqual(this.hoveredUnit, unit.id))
        ) {
          const hoverBarPos = relativeTo(unitPos,
            [{ type: "above", amt: 5 }, { type: "left", amt: 5 }],
            0, 0,
          );
          const sprite = this.hoverSpritePool.newSprite(hoverBarPos.xMin, hoverBarPos.yMin, {}, {});
          sprite.inputEnabled = false;
        }

        // HP
        const unitHpPos = relativeTo(unitPos,
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
        const unitChPos = relativeTo(unitPos,
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
            const abPos = createPosition(
              "left", 970 + 170 * unitIndex + 70 * aiPos.x, 70,
              "top", 760 + 70 * aiPos.y, 70,
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
        const triggerPos = statusPos(state, status.id, statusIndex, tagIndex);
        const trSprite = this.triggerPool.newSprite(triggerPos.xMin, triggerPos.yMin, {}, { status });
        if (currentInputType !== undefined && ! matchUserInput(currentInputType, status.id)) {
          trSprite.alpha = 0.3;
        } else {
          trSprite.alpha = 1;
        }

        // hover graphics
        if (
          (this.selectedUnit !== undefined && deepEqual(this.selectedUnit, status.id)) ||
          (this.hoveredUnit !== undefined && deepEqual(this.hoveredUnit, status.id))
        ) {
          this.hoverGraphicsPool.beginFill();
          this.hoverGraphicsPool.lineStyle(4);
          const centerTriggerPos = center(triggerPos);
          this.hoverGraphicsPool.moveTo(centerTriggerPos.x, centerTriggerPos.y);
          if (status.owner.type === "friendly") {
            const ownerPosCenter = center(friendlyUnitPos(state, new EntityId(status.owner.id, "friendly")));
            this.hoverGraphicsPool.lineTo(ownerPosCenter.x, ownerPosCenter.y); 
          }
          this.hoverGraphicsPool.endFill();
        }
      });
    });
  }

  drawAnimControlBtns() {
    const types: ["pause", "play", "fast", "skip"] = ["pause", "play", "fast", "skip"];
    types.map((type, typeIndex) => {
      const pos = createPosition(
        "left", 20 + 61 * typeIndex, 60,
        "top", 50, 60,
      );
      this.animControlBtnPool.newSprite(pos.xMin, pos.yMin, {}, { type });
    });
  }

  drawTreeControlBtns() {
    this.treeControlBtnPool.clear();
    const types: ["remove"] = ["remove"];
    types.map((type, typeIndex) => {
      const pos = createPosition(
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
          (this.selectedUnit !== undefined && deepEqual(this.selectedUnit, unit.id)) ||
          (this.hoveredUnit !== undefined && deepEqual(this.hoveredUnit, unit.id))
        ) {
          const unitPos = friendlyUnitPos(state, unitIndex);
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
          (this.selectedUnit !== undefined && deepEqual(this.selectedUnit, unit.id)) ||
          (this.hoveredUnit !== undefined && deepEqual(this.hoveredUnit, unit.id))
        ) {
          const unitPos = enemyUnitPos(state, unitIndex);
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
        const triggerPos = statusPos(state, status.id, statusIndex, tagIndex);
        // hover graphics
        if (
          (this.selectedUnit !== undefined && deepEqual(this.selectedUnit, status.id)) ||
          (this.hoveredUnit !== undefined && deepEqual(this.hoveredUnit, status.id))
        ) {
          this.hoverGraphicsPool.beginFill();
          this.hoverGraphicsPool.lineStyle(4);
          const centerTriggerPos = center(triggerPos);
          this.hoverGraphicsPool.moveTo(centerTriggerPos.x, centerTriggerPos.y);
          if (status.owner.type === "friendly") {
            const ownerPosCenter = center(friendlyUnitPos(state, new EntityId(status.owner.id, "friendly")));
            this.hoverGraphicsPool.lineTo(ownerPosCenter.x, ownerPosCenter.y); 
          }
          this.hoverGraphicsPool.endFill();
        }
      });
    });

    // DETAIL
    const showUnit = this.hoveredUnit !== undefined ? this.hoveredUnit : this.selectedUnit;
    if (showUnit !== undefined) {
      if (showUnit.type === "status") {
        // compiler does not refine type of this id
        const statusId = showUnit as StatusId;

        const unit: StStatus | undefined = getTarget(state, statusId);
        if (unit !== undefined) {
          const pos1 = createPosition(
            "left", 650, 150,
            "bot", 100, 300,
          );
          this.statsTextPool.newText(pos1, `${unit.tag}: ${unit.value} (${unit.hp})`);
        }
      }
    }
  }

  drawIntermediateActions(
    state: GameState,
    log: Log,
  ): Animation {
    const anims = allLogIndices(log).map(x => {
      return new Create(
        () => { return; },
        () => {
          return this.drawIntermediateAction(x.logIndex);
        }
      );
    });
    return new SeqAnimation(anims);
  }

  drawIntermediateAction(
    intermediate: LogIndex,
  ): Animation {
    this.logActionPool.clear();
    this.logTextPool.clear();
    this.logTextSpritePool.clear();
    this.logTriggerPool.clear();
    
    this.intermediate = intermediate;
    console.log(`INTERMEDIATE: ${JSON.stringify(this.intermediate)}`);
    this.drawCurrentState();

    const state = this.currentState();
    const log = this.log!;
    const logEntry = getLogEntry(log, intermediate);
    const prevLog = getPrevLogEntry(log, intermediate);

    // draw log icons
    const anims: Animation[] = allLogIndices(log).map(x => {
      const entryIndex = x.entryIndex;
      const typeIndex = x.typeIndex;
      const entry = getLogEntry(this.log!, x.logIndex);

      const pos = createPosition(
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

    // draw action popup text
    const action = logEntry.action;
    const actionAnimPos = this.logLocation(action, state);
    const actionAnim = new Create(() => {
      return this.createLogTextSprite(actionAnimPos.xMin, actionAnimPos.yMin, action);
    }, self => {
      return new BaseAnimation(1000, self, t => {
        t.to({ y: actionAnimPos.yMin - 100 }, 1000);
        t.onComplete.add(() => self.destroy());
      });
    });
    
    // draw frames
    const origin = action.origin;
    let frameAnim: Animation[] = [];
    if (origin !== "noOrigin") {
      frameAnim = [this.drawFrames(state, action, origin as any)];
    }
    
    // draw difference with prev log
    if (prevLog !== undefined) {
      // console.log(`PREV: ${JSON.stringify(prevLog.action)}`)
    }

    return new ParAnimation(anims.concat(frameAnim).concat(actionAnim));
  }

  logLocation(
    action: Action,
    state: GameState,
  ): Position {
    switch (action.tag) {
      case "AddStatus": // fallthrough
      case "UseCharge": // fallthrough
      case "Damage": {
        return this.onTargetPos(state, action.target);
      }
      case "AddThreat": {
        return this.onTargetPos(state, action.forAlly);
      }
      default: {
        return createPosition(
          "left", 380, 100,
          "top", 200, 100,
        );
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

        return statusPos(state, id);
      }
      case "enemy": {
        const id = _id as EnemyId;

        return enemyUnitPos(state, id);
      }
      case "friendly": {
        const id = _id as FriendlyId;

        return friendlyUnitPos(state, id);
      }
    }
  }

  createLogTextSprite(
    parentX: number,
    parentY: number,
    action: Action,
  ): Phaser.Sprite {
    const desc = actionDescription(action);
    let y = 0;
    let xOffset = 0;
    const dataList: {
      x: number,
      y: number,
      frameType: {},
      data: LogTextSpriteData,
    }[] = [];
    desc.forEach((descSym, descIndex) => {
      switch (descSym.tag) {
        case "DescSeparator": {
          y += 1;
          xOffset = descIndex + 1;
          break;
        }
        case "DescSymbol": {
          const xPos = 80 * (descIndex - xOffset);
          const yPos = - y * 80;
          dataList.push({ x: xPos, y: yPos, frameType: {}, data: { sprite: descSym.sym } });
          break;
        }
      }
    });
    return this.logTextSpritePool.newGroup(parentX, parentY, dataList);
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
          const textPos = createPosition(
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

  setLogAnimationSpeed(
    speed: SpeedType,
  ) {
    this.gameRefs.saveData.act.animationSpeeds.log = speed;
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
): Pool<{}, "neutral" | "hover" | "down"> {
  return mkButtonPool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        switch (frameType) {
          case "down": return "btn_level_click.png";
          case "hover": return "btn_level_hover.png";
          case "neutral": return "btn_level_neutral.png";
        }
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 50, Phaser.Easing.Linear.None, false, 0);
        }
      ],
      callbacks: {
        click: (self) => {
          clearSolution(gameRefs);
        },
      },
    },
    self => { return false; }
  );
}

type UnitData = {
  cardId: string,
  globalId: EntityId<"friendly" | "enemy">,
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
          const clickState = gameRefs.screens.execScreen.clickState;
          if (clickState === undefined) {
            clickUnit(gameRefs, self.data.globalId);
          } else {
            const inputType: UserInput = clickState.ability.inputs[clickState.inputs.length];
            if (matchUserInput(inputType, self.data.globalId)) {
              clickState.inputs.push(self.data.globalId);
              if (clickState.inputs.length === clickState.ability.inputs.length) {
                extendLevelSolution(gameRefs, gameRefs.screens.execScreen.solDataFromClickState());
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
          if (clickState === undefined) {
            clickUnit(gameRefs, self.data.globalId);
          } else {
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
              gameRefs.screens.execScreen.clickState === undefined
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
          if (self.data.tag === "FrAbilityData" || self.data.tag === "EnAbilityData") {
            let intent: Ability;
            if (self.data.tag === "FrAbilityData") {
              intent = self.data.ability.ability;
            } else {
              intent = self.data.ai.ability;
            }
            const desc = intentDescription(intent);
            let y = 0;
            let xOffset = 0;
            desc.forEach((descSym, descIndex) => {
              const explPos = createPosition(
                "left", 750 + 80 * (descIndex - xOffset), 80,
                "bot", 225 - y * 80, 80,
              );
              switch (descSym.tag) {
                case "DescSeparator": {
                  y += 1;
                  xOffset = descIndex + 1;
                  break;
                }
                case "DescSymbol": {
                  gameRefs.screens.execScreen.detailExplPool.newSprite(explPos.xMin, explPos.yMin, {}, { sprite: descSym.sym });
                  break;
                }
              }
            });

            y += 1;
          }
        },
        hoverOut: (self) => {
          gameRefs.screens.execScreen.detailExplPool.clear();
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
          case "Armor": return "icon_armor.png";
          case "Weak": return "icon_weak.png";
          case "Strong": return "icon_strong.png";
          case "Fragile": return "icon_fragile.png";
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
          if (clickState === undefined) {
            clickUnit(gameRefs, globalId);
          } else {
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
          case "Damage": return "icon_hp_minus.png";
          case "UseCharge": return "icon_ch_minus.png";
          case "AddThreat": return "icon_th_plus.png";
          case "MoveAI": return "icon_ai.png";
          case "StartTurn": return "icon_start_turn.png";
          case "AddStatus": return "icon_add_status.png";
          case "Death": return "icon_death.png";
          default: return "icon_b.png";
        }
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 1000, Phaser.Easing.Linear.None, false, 5);
          (<any>tween).data = { log: true };
          tween.timeScale = speedTypeToSpeed(gameRefs.saveData.act.animationSpeeds.log);
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
          //drawDescriptionToHoverInfo(gameRefs, actionDescription(self.data.action), 50, 0);
          self.data.transforms.forEach((transform, transformIndex) => {
            drawDescriptionToHoverInfo(gameRefs, actionDescription(transform.before), 50, 50 * transformIndex);
            if (transformIndex === self.data.transforms.length - 1) {
              drawDescriptionToHoverInfo(gameRefs, actionDescription(transform.after), 50, 50 * (transformIndex + 1));
            }
            drawDescriptionToHoverInfo(gameRefs, triggerTagDescription(transform.tag), 0, 50 * (transformIndex + 1));
          });
        },
        hoverOut: (self) => {
          gameRefs.screens.execScreen.drawCurrentState();
        },
      },
    },
  );
}

function drawDescriptionToHoverInfo(
  gameRefs: GameRefs,
  desc: DescToken[],
  startX: number = 0,
  startY: number = 0,
) {
  let y = 0;
  let xOffset = 0;
  desc.forEach((descSym, descIndex) => {
    const explPos = createPosition(
      "left", 750 + startX + 80 * (descIndex - xOffset), 80,
      "bot", 225 - startY - y * 80, 80,
    );
    switch (descSym.tag) {
      case "DescSeparator": {
        y += 1;
        xOffset = descIndex + 1;
        break;
      }
      case "DescSymbol": {
        gameRefs.screens.execScreen.detailExplPool.newSprite(explPos.xMin, explPos.yMin, {}, { sprite: descSym.sym });
        break;
      }
    }
  });
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
          tween.timeScale = speedTypeToSpeed(gameRefs.saveData.act.animationSpeeds.log);
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
            case "remove": return "icon_anim_pause.png";
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