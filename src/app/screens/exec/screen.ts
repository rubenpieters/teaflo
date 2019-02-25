import { Pool, mkButtonPool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { createPosition, relativeTo, Position } from "../../util/position";
import { addText, DataSprite } from "../../phaser/datasprite";
import { GameState, filteredEn, filteredFr, FrStUnit, EnStUnit, findStatus } from "../../../shared/game/state";
import { Log, LogEntry, LogKeys, LogIndex, LogKeySt, LogKeyFr, LogKeyEn, allLogIndices, getLogEntry, logIndexLt, logIndexEq, nextLogKey, getPrevLogEntry } from "../../../shared/game/log";
import { cardMap } from "../../../app/data/cardMap";
import { TextPool } from "../../phaser/textpool";
import { getUnit, GlobalId, UnitId, getStatus, findIndex, TargetId, eqUnitId } from "../../../shared/game/entityId";
import { hoverUnit, clearHover, clickUnit, extendLevelSolution, changeLevelLoc, clearSolution } from "./events";
import { Ability, UserInput, matchUserInput } from "../../../shared/game/ability";
import { triggerOrder, StTrigger, Trigger, TriggerLog } from "../../../shared/game/trigger";
import { Action } from "../../../shared/game/action";
import { chainSpriteCreation, createTween, addTextPopup, speedTypeToSpeed, SpeedType, addSpritePopup } from "../../../app/phaser/animation";
import { drawPositions, Location } from "../../../shared/tree";
import { Solution } from "../../../shared/game/solution";
import { intentDescription, actionDescription } from "../../util/intentDesc";
import { transitionScreen, ScreenCodex } from "../transition";
import { CodexTypes } from "../codex/screen";

export type UnitSelection = GlobalId<"friendly" | "enemy"> | GlobalId<"status">;

export class ExecScreen {
  clearBtnPool: Pool<{}, "neutral" | "hover" | "down">
  unitPool: Pool<UnitData, {}>
  unitResPool: Pool<UnitResData, {}>
  abilityPool: Pool<AbilityData, {}>
  unitTextPool: TextPool
  statsTextPool: TextPool
  triggerPool: Pool<TriggerData, {}>
  logTextPool: TextPool
  logTextSpritePool: Pool<LogTextSpriteData, {}>
  logActionPool: Pool<LogActionData, {}>
  logTriggerPool: Pool<LogTriggerData, {}>
  solTreePool: Pool<SolTreeData, {}>
  detailBtnPool: Pool<DetailBtnData, {}>
  detailExplPool: Pool<DetailExplData, {}>
  hoverSpritePool: Pool<HoverSpriteData, {}>

  animControlBtnPool: Pool<AnimControlBtn, {}>

  state: GameState | undefined
  log: Log | undefined
  hoveredUnit: UnitSelection | undefined
  selectedUnit: UnitSelection | undefined
  showAbilityIndex: number | undefined
  clickState: { ability: Ability, inputs: any[], origin: UnitId } | undefined
  intermediate: LogIndex | undefined

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.clearBtnPool = mkClearBtnPool(gameRefs);
    this.unitPool = mkUnitPool(gameRefs);
    this.unitResPool = mkUnitResPool(gameRefs);
    this.unitTextPool = new TextPool(gameRefs.game);
    this.statsTextPool = new TextPool(gameRefs.game);
    this.abilityPool = mkAbilityPool(gameRefs);
    this.triggerPool = mkTriggerPool(gameRefs);
    this.logTextPool = new TextPool(gameRefs.game);
    this.logActionPool = mkLogActionPool(gameRefs);
    this.logTriggerPool = mkLogTriggerPool(gameRefs);
    this.animControlBtnPool = mkAnimControlBtnPool(gameRefs);
    this.solTreePool = mkSolTreePool(gameRefs);
    this.detailBtnPool = mkDetailBtnPool(gameRefs);
    this.detailExplPool = mkDetailExplPool(gameRefs);
    this.logTextSpritePool = mkLogTextSpritePool(gameRefs);
    this.hoverSpritePool = mkHoverSpritePool(gameRefs);
  }

  reset() {
    this.state = undefined;
    this.log = undefined;
    this.hoveredUnit = undefined;
    this.selectedUnit = undefined;
    this.showAbilityIndex = undefined;
    this.clickState = undefined;
    this.intermediate = undefined;
    this.logActionPool.clear();
    this.logTextPool.clear();
    this.logTextSpritePool.clear();
    this.logTriggerPool.clear();
  }

  canExtendState(): boolean {
    if (this.state === undefined) {
      return false;
    }
    if (this.state.state === "invalid" || this.state.state === "win") {
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

  drawState(
    state: GameState,
    prevState?: GameState,
  ) {
    // when drawing elements: if difference with previous state, play animation
    this.unitPool.clear();
    this.unitResPool.clear();
    this.triggerPool.clear();
    this.unitTextPool.clear();
    this.hoverSpritePool.clear();

    // calculate max threat value
    const enIds = filteredEn(state)
      .map(x => x.id)
      ;
    const maxThreat = filteredFr(state)
      .map(x => Object.values(x.threatMap))
      .reduce((acc, curr) => Math.max(...curr.concat(acc)), 1)
      ;

    state.frUnits.forEach((unit, unitIndex) => {
      if (unit !== undefined) {
        const unitPos = createPosition(
          "left", 250 + 170 * unitIndex, 150,
          "top", 20, 150,
        );
        const unitSprite = this.unitPool.newSprite(unitPos.xMin, unitPos.yMin, {},
          { cardId: unit.cardId,
            globalId: new GlobalId(unit.id, "friendly"),
          }
        );

        // hover bar
        if (
          (this.selectedUnit !== undefined && eqUnitId(state, this.selectedUnit, new GlobalId(unit.id, "friendly"))) ||
          (this.hoveredUnit !== undefined && eqUnitId(state, this.hoveredUnit, new GlobalId(unit.id, "friendly")))
        ) {
          const hoverBarPos = createPosition(
            "left", 245 + 170 * unitIndex, 10,
            "top", 15, 150,
          );
          const sprite = this.hoverSpritePool.newSprite(hoverBarPos.xMin, hoverBarPos.yMin, {}, {});
          sprite.inputEnabled = false;
        }

        // HP
        const unitHpPos = createPosition(
          "left", 245 + 170 * unitIndex, 10,
          "top", 20, 150,
        );
        const hpHeight = unitHpPos.yMax - unitHpPos.yMin;
        unitHpPos.yMin = unitHpPos.yMin + hpHeight * ((unit.maxHp - unit.hp) / unit.maxHp);
        const unitHpSprite = this.unitResPool.newSprite(unitHpPos.xMin, unitHpPos.yMin, {},
          { cardId: unit.cardId,
            globalId: new GlobalId(unit.id, "friendly"),
            type: "hp",
          }
        );
        unitHpSprite.width = 10;
        unitHpSprite.height = hpHeight * (unit.hp / unit.maxHp);
        // draw hp changing animation
        if (prevState !== undefined) {
          const prevUnit = getUnit(new GlobalId(unit.id, "friendly"), prevState);
          if (prevUnit !== undefined && prevUnit.hp !== unit.hp) {
            const prevUnitHpPos = createPosition(
              "left", 245 + 170 * unitIndex, 10,
              "top", 20, 150,
            );
            const prevDiff = hpHeight * ((prevUnit.maxHp - prevUnit.hp) / prevUnit.maxHp);
            prevUnitHpPos.yMax = unitHpPos.yMin;
            prevUnitHpPos.yMin = prevUnitHpPos.yMin + prevDiff;
            const prevUnitHpSprite = this.unitResPool.newSprite(prevUnitHpPos.xMin, prevUnitHpPos.yMin, {},
              { cardId: unit.cardId,
                globalId: new GlobalId(unit.id, "friendly"),
                type: "res_anim",
              }
            );
            prevUnitHpSprite.width = 10;
            prevUnitHpSprite.height = prevUnitHpPos.yMax - prevUnitHpPos.yMin;
            const tween = createTween(this.gameRefs.game, prevUnitHpSprite,
              tween => {
                tween.to({ y: prevUnitHpPos.yMax, height: 0 }, 200, undefined, false, 100);
              }
            );
            tween.onComplete.add(() => prevUnitHpSprite.kill());
            tween.start();
          }
        }

        // CH
        const unitChPos = createPosition(
          "left", 395 + 170 * unitIndex, 10,
          "top", 20, 150,
        );
        const chHeight = unitChPos.yMax - unitChPos.yMin;
        unitChPos.yMin = unitChPos.yMin + chHeight * ((unit.maxCharges - unit.charges) / unit.maxCharges);
        const unitChSprite = this.unitResPool.newSprite(unitChPos.xMin, unitChPos.yMin, {},
          { cardId: unit.cardId,
            globalId: new GlobalId(unit.id, "friendly"),
            type: "ch",
          }
        );
        unitChSprite.width = 10;
        unitChSprite.height = chHeight * (unit.charges / unit.maxCharges);

        // TH
        enIds.forEach((enId, enIndex) => {
          const unitThPos = createPosition(
            "left", 245 + 170 * unitIndex + 30 * enIndex, 25,
            "top", 240, 100,
          );
          const unitThSprite = this.unitResPool.newSprite(unitThPos.xMin, unitThPos.yMin, {},
            { cardId: unit.cardId,
              globalId: new GlobalId(unit.id, "friendly"),
              type: "th",
            }
          );
          const threat = unit.threatMap[enId] === undefined ? 0 : unit.threatMap[enId];
          unitThSprite.width = 25;
          unitThSprite.height = threat / maxThreat * 50;
        });
      }
    });

    state.enUnits.forEach((unit, unitIndex) => {
      if (unit !== undefined) {
        const unitPos = createPosition(
          "left", 1000 + 170 * unitIndex, 150,
          "top", 20, 150,
        );
        const unitSprite = this.unitPool.newSprite(unitPos.xMin, unitPos.yMin, {},
          { cardId: unit.cardId,
            globalId: new GlobalId(unit.id, "enemy"),
          }
        );

        // hover bar
        if (
          (this.selectedUnit !== undefined && eqUnitId(state, this.selectedUnit, new GlobalId(unit.id, "enemy"))) ||
          (this.hoveredUnit !== undefined && eqUnitId(state, this.hoveredUnit, new GlobalId(unit.id, "enemy")))
        ) {
          const hoverBarPos = createPosition(
            "left", 995 + 170 * unitIndex, 10,
            "top", 15, 150,
          );
          const sprite = this.hoverSpritePool.newSprite(hoverBarPos.xMin, hoverBarPos.yMin, {}, {});
          sprite.inputEnabled = false;
        }

        // HP
        const unitHpPos = createPosition(
          "left", 995 + 170 * unitIndex, 10,
          "top", 20, 150,
        );
        const hpHeight = unitHpPos.yMax - unitHpPos.yMin;
        unitHpPos.yMin = unitHpPos.yMin + hpHeight * ((unit.maxHp - unit.hp) / unit.maxHp);
        const unitHpSprite = this.unitResPool.newSprite(unitHpPos.xMin, unitHpPos.yMin, {},
          { cardId: unit.cardId,
            globalId: new GlobalId(unit.id, "enemy"),
            type: "hp",
          }
        );
        unitHpSprite.width = 10;
        unitHpSprite.height = hpHeight * (unit.hp / unit.maxHp);
        // draw hp changing animation
        if (prevState !== undefined) {
          const prevUnit = getUnit(new GlobalId(unit.id, "enemy"), prevState);
          if (prevUnit !== undefined && prevUnit.hp !== unit.hp) {
            const prevUnitHpPos = createPosition(
              "left", 995 + 170 * unitIndex, 10,
              "top", 20, 150,
            );
            const prevDiff = hpHeight * ((prevUnit.maxHp - prevUnit.hp) / prevUnit.maxHp);
            prevUnitHpPos.yMax = unitHpPos.yMin;
            prevUnitHpPos.yMin = prevUnitHpPos.yMin + prevDiff;
            const prevUnitHpSprite = this.unitResPool.newSprite(prevUnitHpPos.xMin, prevUnitHpPos.yMin, {},
              { cardId: unit.cardId,
                globalId: new GlobalId(unit.id, "enemy"),
                type: "res_anim",
              }
            );
            prevUnitHpSprite.width = 10;
            prevUnitHpSprite.height = prevUnitHpPos.yMax - prevUnitHpPos.yMin;
            const tween = createTween(this.gameRefs.game, prevUnitHpSprite,
              tween => {
                tween.to({ y: prevUnitHpPos.yMax, height: 0 }, 200, undefined, false, 100);
              }
            );
            tween.onComplete.add(() => prevUnitHpSprite.kill());
            tween.start();
          }
        }

        // CH
        const unitChPos = createPosition(
          "left", 1145 + 170 * unitIndex, 10,
          "top", 20, 150,
        );
        const chHeight = unitChPos.yMax - unitChPos.yMin;
        unitChPos.yMin = unitChPos.yMin + chHeight * ((unit.maxCharges - unit.charges) / unit.maxCharges);
        const unitChSprite = this.unitResPool.newSprite(unitChPos.xMin, unitChPos.yMin, {},
          { cardId: unit.cardId,
            globalId: new GlobalId(unit.id, "enemy"),
            type: "ch",
          }
        );
        unitChSprite.width = 10;
        unitChSprite.height = chHeight * (unit.charges / unit.maxCharges);
      }
    });

    // AETHER
    triggerOrder.forEach((tag, tagIndex) => {
      state.triggers[tag].forEach((trigger, triggerIndex) => {
        const triggerPos = createPosition(
          "left", 240 + 50 * triggerIndex, 40,
          "top", 400 + 50 * tagIndex, 40,
        );
        this.triggerPool.newSprite(triggerPos.xMin, triggerPos.yMin, {}, { trigger });
      });
    });
  }

  drawAnimControlBtns() {
    const types: ["pause", "play", "fast", "skip"] = ["pause", "play", "fast", "skip"];
    types.map((type, typeIndex) => {
      const pos = createPosition(
        "left", 50 + 70 * typeIndex, 60,
        "bot", 50, 60,
      );
      this.animControlBtnPool.newSprite(pos.xMin, pos.yMin, {}, { type });
    });
  }

  drawStats(
    state: GameState,
  ) {
    this.abilityPool.clear();
    this.statsTextPool.clear();
    this.detailBtnPool.clear();
    this.detailExplPool.clear();
    this.hoverSpritePool.clear();

    // hover bar
    state.frUnits.forEach((unit, unitIndex) => {
      if (unit !== undefined) {
        // hover bar
        if (
          (this.selectedUnit !== undefined && eqUnitId(state, this.selectedUnit, new GlobalId(unit.id, "friendly"))) ||
          (this.hoveredUnit !== undefined && eqUnitId(state, this.hoveredUnit, new GlobalId(unit.id, "friendly")))
        ) {
          const hoverBarPos = createPosition(
            "left", 245 + 170 * unitIndex, 10,
            "top", 15, 150,
          );
          const sprite = this.hoverSpritePool.newSprite(hoverBarPos.xMin, hoverBarPos.yMin, {}, {});
          sprite.inputEnabled = false;
        }
      }
    });
    state.enUnits.forEach((unit, unitIndex) => {
      if (unit !== undefined) {
        // hover bar
        if (
          (this.selectedUnit !== undefined && eqUnitId(state, this.selectedUnit, new GlobalId(unit.id, "enemy"))) ||
          (this.hoveredUnit !== undefined && eqUnitId(state, this.hoveredUnit, new GlobalId(unit.id, "enemy")))
        ) {
          const hoverBarPos = createPosition(
            "left", 995 + 170 * unitIndex, 10,
            "top", 15, 150,
          );
          const sprite = this.hoverSpritePool.newSprite(hoverBarPos.xMin, hoverBarPos.yMin, {}, {});
          sprite.inputEnabled = false;
        }
      }
    });

    // DETAIL
    const showUnit = this.hoveredUnit !== undefined ? this.hoveredUnit : this.selectedUnit;
    if (showUnit !== undefined) {
      if (showUnit.type === "friendly" || showUnit.type === "enemy") {
        const unit = getUnit(showUnit, state);
        if (unit !== undefined) {
          // hp/ch/th text
          const pos1 = createPosition(
            "left", 650, 150,
            "bot", 200, 300,
          );
          this.statsTextPool.newText(pos1, `${unit.hp} / ${unit.maxHp}`);
          const pos2 = createPosition(
            "left", 850, 150,
            "bot", 200, 300,
          );
          this.statsTextPool.newText(pos2, `${unit.charges} / ${unit.maxCharges}`);
  
          if (showUnit.type === "friendly") {
            const frUnit = <FrStUnit>unit;

            // go to codex button
            const detailBtnPos = createPosition(
              "left", 350, 150,
              "bot", 100, 150,
            );
            this.detailBtnPool.newSprite(detailBtnPos.xMin, detailBtnPos.yMin, {}, { type: { tag: "FrCardId", cardId: frUnit.cardId } });

            // abilities
            frUnit.abilities.forEach((ability, abilityIndex) => {
              const flagTop = abilityIndex % 2;
              const flagLeft = abilityIndex < 2 ? 0 : 1;
              if (abilityIndex >= 4) { throw "ability index should be below 4" };
              const abPos = createPosition(
                "left", 500 + 125 * flagLeft, 100,
                "bot", 200 - 125 * flagTop, 100,
              );
              this.abilityPool.newSprite(abPos.xMin, abPos.yMin, {}, { tag: "FrAbilityData", spriteId: ability.spriteId, ability, index: abilityIndex, globalId:  new GlobalId(unit.id, "friendly") });

              if (this.showAbilityIndex !== undefined && this.showAbilityIndex === abilityIndex) {
                const desc = intentDescription(ability.intent);
                let y = 0;
                let xOffset = 0;
                desc.forEach((descSym, descIndex) => {
                  const explPos = createPosition(
                    "left", 750 + 80 * (descIndex - xOffset), 80,
                    "bot", 250 - y * 80, 80,
                  );
                  switch (descSym.tag) {
                    case "DescSeparator": {
                      y += 1;
                      xOffset = descIndex + 1;
                      break;
                    }
                    case "DescSymbol": {
                      this.detailExplPool.newSprite(explPos.xMin, explPos.yMin, {}, { sprite: descSym.sym });
                      break;
                    }
                  }
                });
              }
            });
          } else if (showUnit.type === "enemy") {
            const enUnit = <EnStUnit>unit;

            // go to codex button
            const detailBtnPos = createPosition(
              "left", 350, 150,
              "bot", 100, 150,
            );
            this.detailBtnPool.newSprite(detailBtnPos.xMin, detailBtnPos.yMin, {}, { type: { tag: "EnCardId", cardId: enUnit.cardId } });

            const abPos = createPosition(
              "left", 500, 100,
              "bot", 200, 100,
            );
            const ability = enUnit.ai[enUnit.currentAI];
            this.abilityPool.newSprite(abPos.xMin, abPos.yMin, {}, { tag: "EnAbilityData", spriteId: ability.spriteId });

            const desc = intentDescription(ability.intent);
            let y = 0;
            let xOffset = 0;
            desc.forEach((descSym, descIndex) => {
              const explPos = createPosition(
                "left", 750 + 80 * (descIndex - xOffset), 80,
                "bot", 250 - y * 80, 80,
              );
              switch (descSym.tag) {
                case "DescSeparator": {
                  y += 1;
                  xOffset = descIndex + 1;
                  break;
                }
                case "DescSymbol": {
                  this.detailExplPool.newSprite(explPos.xMin, explPos.yMin, {}, { sprite: descSym.sym });
                  break;
                }
              }
            });
          }
        }
      } else {
        const unit = getStatus(<GlobalId<"status">>showUnit, state);
        if (unit !== undefined) {
          const pos1 = createPosition(
            "left", 650, 150,
            "bot", 100, 300,
          );
          this.statsTextPool.newText(pos1, `${unit.tag}: ${unit.fragments}`);
        }
      }
    }
  }

  drawIntermediateLog(
    upto: LogIndex,
    animation: boolean,
  ) {
    this.logActionPool.clear();
    this.logTextPool.clear();
    this.logTextSpritePool.clear();
    this.logTriggerPool.clear();

    this.intermediate = upto;

    let spriteFs: {
      create: () => DataSprite<any>,
      introTween: (sprite: DataSprite<any>) => { first: Phaser.Tween, last: Phaser.Tween } | undefined,
    }[] = [];

    const state = this.currentState();
    const log = this.log!;
    allLogIndices(state, log).forEach(x => {
      const entryIndex = x.entryIndex;
      const typeIndex = x.typeIndex;
      const entry = getLogEntry(this.log!, x.logIndex);
      const pos = createPosition(
        "left", 20 + 50 * entryIndex, 40,
        "top", 20 + 80 * typeIndex, 40,
      );
      if (logIndexLt(x.logIndex, upto)) {
        const sprite = this.logActionPool.newSprite(pos.xMin, pos.yMin, {}, {...entry, ...{ logIndex: x.logIndex }});
        sprite.alpha = 1;
      } else if (logIndexEq(x.logIndex, upto)) {
        const logAction = this.createLogEntryAnim(entry, x.logIndex, pos, undefined);
        spriteFs.push(logAction);
        entry.transforms.forEach((triggerLog, triggerLogIndex) => {
          const pos = createPosition(
            "left", 390, 150,
            "top", 40 + triggerLogIndex * 70, 300,
          );
          const logTrigger = this.createTriggerEntryAnim(triggerLog, pos);
          spriteFs.push(logTrigger);
        });
        const nxtLogKey = nextLogKey(state, log, upto);
        if (animation && nxtLogKey !== undefined) {
          const last = {
            create: () => {
              this.drawIntermediateLog(nxtLogKey, true);
              return <any>undefined;
            },
            introTween: (sprite: DataSprite<LogActionData>) => {
              return undefined;
            },
          };
          spriteFs.push(last);
        }
      } else {
        const sprite = this.logActionPool.newSprite(pos.xMin, pos.yMin, {}, {...entry, ...{ logIndex: x.logIndex }});
        sprite.alpha = 0.5;
      }
    });
    this.drawCurrentState();
    chainSpriteCreation(spriteFs, animation);
  }

  createLogEntryAnim(
    entry: LogEntry,
    logIndex: LogIndex,
    pos: Position,
    prevState: GameState | undefined,
    tint: number = 0xFFFFFF,
  ) {
    return {
      create: () => {
        const sprite = this.logActionPool.newSprite(pos.xMin, pos.yMin, {}, {...entry, ...{ logIndex }});
        sprite.tint = tint;
        return sprite;
      },
      introTween: (sprite: DataSprite<LogActionData>) => {
        const tween = this.logActionPool.introTween(sprite);
        if (tween !== undefined) {
          const loc = this.logLocation(entry.action, entry.state);
          addSpritePopup(
            this.gameRefs,
            tween.first,
            () => {
              const parent = this.logTextSpritePool.newSprite(loc.xMin, loc.yMin, {}, { sprite: "btn_level_neutral.png" });
              this.createLogTextSprite(entry.action, parent);
              return parent;
            },
            tween => {
              tween.to({ y: loc.yMin - 100 }, 1000);
            },
            "log",
          );
          tween.first.onStart.add(() => {
            // clear trigger log entries
            this.gameRefs.screens.execScreen.logTriggerPool.clear();
            // update state view
            const prev = getPrevLogEntry(this.log!, logIndex);
            const prevS = prev === undefined ? prevState : prev.state;
            this.gameRefs.screens.execScreen.drawState(entry.state, prevS);
            this.gameRefs.screens.execScreen.drawStats(entry.state);
          });
        }
        return tween;
      },
    };
  }

  logLocation(
    action: Action,
    state: GameState,
  ): Position {
    switch (action.tag) {
      case "Damage": {
        return this.onTargetPos(state, action.target);
      }
      case "AddThreat": {
        return this.onTargetPos(state, action.toFriendly);
      }
      case "UseCharge": {
        return this.onTargetPos(state, action.target);
      }
      case "AddTrigger": {
        return this.onTargetPos(state, action.target);
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
    id: TargetId,
  ) {
    switch (id.type) {
      case "status": {
        const index = findStatus(state, id);
        return createPosition(
          "left", 650 + 80 * index!.index, 100,
          "top", 400 + 80 * triggerOrder.findIndex(x => x === index!.group), 100,
        );
      }
      case "enemy": {
        const index = findIndex(state, id);
        return createPosition(
          "left", 1300 + 170 * index!, 100,
          "top", 200, 100,
        );
      }
      case "friendly": {
        const index = findIndex(state, id);
        return createPosition(
          "left", 650 + 170 * index!, 100,
          "top", 200, 100,
        );
      }
    }
  }

  createLogTextSprite(
    action: Action,
    parent: Phaser.Sprite,
  ) {
    const desc = actionDescription(action);
    let y = 0;
    let xOffset = 0;
    desc.forEach((descSym, descIndex) => {
      switch (descSym.tag) {
        case "DescSeparator": {
          y += 1;
          xOffset = descIndex + 1;
          break;
        }
        case "DescSymbol": {
          //const xPos = startPos.xMin + 80 * (descIndex - xOffset);
          //const yPos = startPos.yMin - y * 80;
          const xPos = 80 * (descIndex - xOffset);
          const yPos = - y * 80;
          const sprite = this.logTextSpritePool.newSprite(xPos, yPos, {}, { sprite: descSym.sym });
          parent.addChild(sprite);
          break;
        }
      }
    });
  }

  createTriggerEntryAnim(
    triggerLog: TriggerLog,
    pos: Position,
  ) {
    return {
      create: () => {
        const sprite = this.logTriggerPool.newSprite(pos.xMin, pos.yMin, {}, triggerLog);
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
              return this.logTextPool.newText(textPos, `${triggerLog.tag}`);
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

  drawTree(
    solInfo: {
      solution: Solution,
      loc: Location,
    }
  ) {
    this.solTreePool.clear();

    const x = 100;
    const y = 750;

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

  clear() {
    
  }

  clearAnimations() {
    this.gameRefs.game.tweens.removeFrom(this.clearBtnPool, true);
    this.gameRefs.game.tweens.removeFrom(this.unitPool, true);
    this.gameRefs.game.tweens.removeFrom(this.unitResPool, true);
    this.gameRefs.game.tweens.removeFrom(this.abilityPool, true);
    this.gameRefs.game.tweens.removeFrom(this.triggerPool, true);
    this.gameRefs.game.tweens.removeFrom(this.logActionPool, true);
    this.gameRefs.game.tweens.removeFrom(this.logTriggerPool, true);
    this.gameRefs.game.tweens.removeFrom(this.solTreePool, true);
    this.gameRefs.game.tweens.removeFrom(this.detailBtnPool, true);
    this.gameRefs.game.tweens.removeFrom(this.statsTextPool.texts, true);
    this.gameRefs.game.tweens.removeFrom(this.unitTextPool.texts, true);
    this.gameRefs.game.tweens.removeFrom(this.logTextPool.texts, true);
    this.gameRefs.game.tweens.removeFrom(this.detailExplPool, true);
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
  globalId: GlobalId<"friendly" | "enemy">,
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
                extendLevelSolution(gameRefs, clickState!);
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
  globalId: GlobalId<"friendly" | "enemy">,
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
                extendLevelSolution(gameRefs, clickState!);
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
  ability: Ability,
  spriteId: string,
  index: number,
  globalId: GlobalId<"friendly" | "enemy">,
  tag: "FrAbilityData",
}

type EnAbilityData = {
  spriteId: string,
  tag: "EnAbilityData",
}

type AbilityData = FrAbilityData | EnAbilityData;

function mkAbilityPool(
  gameRefs: GameRefs,
): Pool<AbilityData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return `${self.data.spriteId}.png`;
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 20, Phaser.Easing.Linear.None, false, 5);
        },
      ],
      callbacks: {
        click: (self) => {
          if (self.data.tag === "FrAbilityData") {
            gameRefs.screens.execScreen.showAbilityIndex = self.data.index;
            if (gameRefs.screens.execScreen.canExtendState() && gameRefs.screens.execScreen.clickState === undefined) {
              gameRefs.screens.execScreen.clickState = {
                ability: self.data.ability,
                inputs: [],
                origin: self.data.globalId,
              }
              if (self.data.ability.inputs.length === 0) {
                extendLevelSolution(gameRefs, gameRefs.screens.execScreen.clickState!);
              }
            }
          }
        },
        hoverOver: (self) => {
          if (self.data.tag === "FrAbilityData") {
            gameRefs.screens.execScreen.showAbilityIndex = self.data.index;
            gameRefs.screens.execScreen.drawStats(gameRefs.screens.execScreen.currentState());
          }
        },
        hoverOut: (self) => {
          if (self.data.tag === "FrAbilityData") {
            gameRefs.screens.execScreen.showAbilityIndex = undefined;
            gameRefs.screens.execScreen.drawStats(gameRefs.screens.execScreen.currentState());
          }
        },
      },
    },
  );
}

type TriggerData = {
  trigger: StTrigger,
};

function mkTriggerPool(
  gameRefs: GameRefs,
): Pool<TriggerData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        switch (self.data.trigger.tag) {
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
          const globalId = new GlobalId(self.data.trigger.id, "status");
          const clickState = gameRefs.screens.execScreen.clickState;
          if (clickState === undefined) {
            clickUnit(gameRefs, globalId);
          } else {
            const inputType: UserInput = clickState.ability.inputs[clickState.inputs.length];
            if (matchUserInput(inputType, globalId)) {
              clickState.inputs.push(globalId);
              if (clickState.inputs.length === clickState.ability.inputs.length) {
                extendLevelSolution(gameRefs, clickState!);
              }
            }
          }
        },
        hoverOver: (self) => {
          hoverUnit(gameRefs, new GlobalId(self.data.trigger.id, "status"));
        },
        hoverOut: (self) => {
          clearHover(gameRefs);
        },
      },
    },
  );
}

type LogActionData = {
  action: Action,
  state: GameState,
  logIndex: LogIndex,
  transforms: TriggerLog[],
};

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
          case "NextAI": return "icon_ai.png";
          case "StartTurn": return "icon_start_turn.png";
          case "AddTrigger": return "icon_add_status.png";
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
          gameRefs.screens.execScreen.drawIntermediateLog(
            self.data.logIndex, false,
          );
        },
        hoverOver: (self) => {
          gameRefs.screens.execScreen.drawState(self.data.state);
          gameRefs.screens.execScreen.drawStats(self.data.state);
        },
        hoverOut: (self) => {
          gameRefs.screens.execScreen.drawCurrentState();
        },
      },
    },
  );
}

type LogTriggerData = {
  tag: Trigger["tag"],
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
          changeLevelLoc(gameRefs, self.data.loc);
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