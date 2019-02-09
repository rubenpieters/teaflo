import { Pool, mkButtonPool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { createPosition, relativeTo, Position } from "../../util/position";
import { addText, DataSprite } from "../../phaser/datasprite";
import { GameState, filteredEn, filteredFr, FrStUnit, EnStUnit } from "../../../shared/game/state";
import { Log, LogEntry } from "../../../shared/game/log";
import { cardMap } from "../../../app/data/cardMap";
import { TextPool } from "../../phaser/textpool";
import { getUnit, GlobalId, UnitId, getStatus } from "../../../shared/game/entityId";
import { hoverUnit, clearHover, clickUnit, extendLevelSolution } from "./events";
import { Ability } from "../../../shared/game/ability";
import { triggerOrder, StTrigger, Trigger, TriggerLog } from "../../../shared/game/trigger";
import { Action } from "../../../shared/game/action";
import { chainSpriteCreation, createTween, addTextPopup, speedTypeToSpeed, SpeedType } from "../../../app/phaser/animation";
import { drawPositions, Location } from "../../../shared/tree";
import { Solution } from "../../../shared/game/solution";

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
  logActionPool: Pool<LogActionData, {}>
  logTriggerPool: Pool<LogTriggerData, {}>
  solTreePool: Pool<SolTreeData, {}>

  animControlBtnPool: Pool<AnimControlBtn, {}>

  state: GameState | undefined
  log: Log | undefined
  hoveredUnit: UnitSelection | undefined
  selectedUnit: UnitSelection | undefined
  clickState: { ability: Ability, inputs: any[], origin: UnitId } | undefined

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
  }

  reset() {
    this.state = undefined;
    this.hoveredUnit = undefined;
    this.selectedUnit = undefined;
    this.clickState = undefined;
  }

  drawClearBtn(
  ) {
    this.redrawExecStartBtn();
    this.clearBtnPool.playIntroAnimations();
  }

  redrawExecStartBtn(
  ) {
    this.clearBtnPool.clear();

    const pos = createPosition(
      "right", 400, 400,
      "bot", 300, 200,
    );
    const sprite = this.clearBtnPool.newSprite(pos.xMin, pos.yMin, "neutral", {});
    addText(this.gameRefs, sprite, pos, "Clear Solution", "#000000", 40);
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
          "left", 650 + 160 * unitIndex, 150,
          "top", 50, 300,
        );
        const unitSprite = this.unitPool.newSprite(unitPos.xMin, unitPos.yMin, {},
          { cardId: unit.cardId,
            globalId: new GlobalId(unit.id, "friendly"),
          }
        );

        // HP
        const unitHpPos = createPosition(
          "left", 650 + 160 * unitIndex, 75,
          "top", 50, 300,
        );
        const hpHeight = unitHpPos.yMax - unitHpPos.yMin;
        unitHpPos.yMin = unitHpPos.yMin + hpHeight * ((unit.maxHp - unit.hp) / unit.maxHp);
        const unitHpSprite = this.unitResPool.newSprite(unitHpPos.xMin, unitHpPos.yMin, {},
          { cardId: unit.cardId,
            globalId: new GlobalId(unit.id, "friendly"),
            type: "hp",
          }
        );
        unitHpSprite.height = hpHeight * (unit.hp / unit.maxHp);
        // draw hp changing animation
        if (prevState !== undefined) {
          const prevUnit = getUnit(new GlobalId(unit.id, "friendly"), prevState);
          if (prevUnit !== undefined && prevUnit.hp !== unit.hp) {
            const prevUnitHpPos = createPosition(
              "left", 650 + 160 * unitIndex, 75,
              "top", 50, 300,
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
          "left", 725 + 160 * unitIndex, 75,
          "top", 50, 300,
        );
        const chHeight = unitChPos.yMax - unitChPos.yMin;
        unitChPos.yMin = unitChPos.yMin + chHeight * ((unit.maxCharges - unit.charges) / unit.maxCharges);
        const unitChSprite = this.unitResPool.newSprite(unitChPos.xMin, unitChPos.yMin, {},
          { cardId: unit.cardId,
            globalId: new GlobalId(unit.id, "friendly"),
            type: "ch",
          }
        );
        unitChSprite.height = chHeight * (unit.charges / unit.maxCharges);

        // TH
        /*let i = 0;
        for (const enId of enIds) {
          const unitThPos = relativeTo(unitPos,
            "below", 250 + 100 * i,
            150, 50,
          );
          unitThPos.xMax = unitThPos.xMin + (unitThPos.xMax - unitThPos.xMin) * (unit.threatMap[enId] / maxThreat);
          createUnitResource(game, gameRefs, unitThPos, "th");
          i += 1;
        }*/
      }
    });
    state.enUnits.forEach((unit, unitIndex) => {
      if (unit !== undefined) {
        const unitPos = createPosition(
          "left", 1300 + 160 * unitIndex, 150,
          "top", 50, 300,
        );
        const unitSprite = this.unitPool.newSprite(unitPos.xMin, unitPos.yMin, {},
          { cardId: unit.cardId,
            globalId: new GlobalId(unit.id, "enemy"),
          }
        );

        // HP
        const unitHpPos = createPosition(
          "left", 1300 + 160 * unitIndex, 75,
          "top", 50, 300,
        );
        const hpHeight = unitHpPos.yMax - unitHpPos.yMin;
        unitHpPos.yMin = unitHpPos.yMin + hpHeight * ((unit.maxHp - unit.hp) / unit.maxHp);
        const unitHpSprite = this.unitResPool.newSprite(unitHpPos.xMin, unitHpPos.yMin, {},
          { cardId: unit.cardId,
            globalId: new GlobalId(unit.id, "enemy"),
            type: "hp",
          }
        );
        unitHpSprite.height = hpHeight * (unit.hp / unit.maxHp);
        // draw hp changing animation
        if (prevState !== undefined) {
          const prevUnit = getUnit(new GlobalId(unit.id, "enemy"), prevState);
          if (prevUnit !== undefined && prevUnit.hp !== unit.hp) {
            const prevUnitHpPos = createPosition(
              "left", 1300 + 160 * unitIndex, 75,
              "top", 50, 300,
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
          "left", 1375 + 160 * unitIndex, 75,
          "top", 50, 300,
        );
        const chHeight = unitChPos.yMax - unitChPos.yMin;
        unitChPos.yMin = unitChPos.yMin + chHeight * ((unit.maxCharges - unit.charges) / unit.maxCharges);
        const unitChSprite = this.unitResPool.newSprite(unitChPos.xMin, unitChPos.yMin, {},
          { cardId: unit.cardId,
            globalId: new GlobalId(unit.id, "enemy"),
            type: "ch",
          }
        );
        unitChSprite.height = chHeight * (unit.charges / unit.maxCharges);
      }
    });

    const textPos = createPosition(
      "left", 650, 150,
      "top", 300, 300,
    );
    this.unitTextPool.newText(textPos, "Aether");
    triggerOrder.forEach((tag, tagIndex) => {
      state.triggers[tag].forEach((trigger, triggerIndex) => {
        const triggerPos = createPosition(
          "left", 650 + 80 * triggerIndex, 150,
          "top", 400 + 80 * tagIndex, 300,
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
    this.clearBtnPool.clear();
    this.abilityPool.clear();
    this.statsTextPool.clear();

    const textPos = createPosition(
      "left", 650, 150,
      "bot", 200, 300,
    );
    this.statsTextPool.newText(textPos, "Detail");

    const showUnit = this.hoveredUnit !== undefined ? this.hoveredUnit : this.selectedUnit;
    if (showUnit !== undefined) {
      if (showUnit.type === "friendly" || showUnit.type === "enemy") {
        const unit = getUnit(showUnit, state);
        if (unit !== undefined) {
          const pos1 = createPosition(
            "left", 650, 150,
            "bot", 100, 300,
          );
          this.statsTextPool.newText(pos1, `${unit.hp} / ${unit.maxHp}`);
  
          if (showUnit.type === "friendly") {
            const frUnit = <FrStUnit>unit;
            frUnit.abilities.forEach((ability, abilityIndex) => {
              const abPos = createPosition(
                "left", 650 + 200 * abilityIndex, 150,
                "bot", 50, 150,
              );
              this.abilityPool.newSprite(abPos.xMin, abPos.yMin, {}, { ability, index: abilityIndex, globalId:  new GlobalId(unit.id, "friendly") });
            });
          } else if (showUnit.type === "enemy") {
            const enUnit = <EnStUnit>unit;
          }
        }
      } else {
        // TODO: breaks if it cannot find status
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

  drawLogAnimation(
    prevState?: GameState | undefined
  ) {
    // on every log action:
    //   intro animation for log action icon
    //   do animation on state
    //   draw new state after that action
    this.logActionPool.clear();
    this.logTextPool.clear();
    let spriteFs: {
      create: () => DataSprite<any>,
      introTween: (sprite: DataSprite<any>) => { first: Phaser.Tween, last: Phaser.Tween } | undefined,
    }[] = [];
    const stF = this.log!.st.forEach((entry: LogEntry, entryIndex) => {
      const pos = createPosition(
        "left", 40 + entryIndex * 70, 150,
        "top", 40, 300,
      );
      spriteFs.push(this.createLogEntryAnim(entry, entryIndex, pos, prevState, "st"));
      entry.transforms.forEach((triggerLog, triggerLogIndex) => {
        const pos = createPosition(
          "left", 390, 150,
          "top", 40, 300,
        );
        spriteFs.push(this.createTriggerEntryAnim(triggerLog, pos));
      });
    });
    const frF = this.log!.fr.map((entry, entryIndex) => {
      const pos = createPosition(
        "left", 40 + entryIndex * 70, 150,
        "top", 150, 300,
      );
      spriteFs.push(this.createLogEntryAnim(entry, entryIndex, pos, undefined, "fr"));
      entry.transforms.forEach((triggerLog, triggerLogIndex) => {
        const pos = createPosition(
          "left", 390, 150,
          "top", 40, 300,
        );
        spriteFs.push(this.createTriggerEntryAnim(triggerLog, pos));
      });
    });
    const enF = this.log!.en.map((entry, entryIndex) => {
      const pos = createPosition(
        "left", 40 + entryIndex * 70, 150,
        "top", 240, 300,
      );
      spriteFs.push(this.createLogEntryAnim(entry, entryIndex, pos, undefined, "en"));
      entry.transforms.forEach((triggerLog, triggerLogIndex) => {
        const pos = createPosition(
          "left", 390, 150,
          "top", 40, 300,
        );
        spriteFs.push(this.createTriggerEntryAnim(triggerLog, pos));
      });
    });
    // show end state at the end
    // added because otherwise the "skip" speed doesn't show the last state for some reason
    const last = {
      create: () => {
        this.gameRefs.screens.execScreen.drawState(this.state!);
        this.gameRefs.screens.execScreen.drawStats(this.state!);
        return <any>undefined;
      },
      introTween: (sprite: DataSprite<LogActionData>) => {
        return undefined;
      },
    };
    spriteFs.push(last);
    chainSpriteCreation(spriteFs, true);
  }

  createLogEntryAnim(
    entry: LogEntry,
    entryIndex: number,
    pos: Position,
    prevState: GameState | undefined,
    type: "st" | "fr" | "en",
  ) {
    return {
      create: () => {
        const sprite = this.logActionPool.newSprite(pos.xMin, pos.yMin, {}, entry);
        return sprite;
      },
      introTween: (sprite: DataSprite<LogActionData>) => {
        const tween = this.logActionPool.introTween(sprite);
        if (tween !== undefined) {
          const textPos = createPosition(
            "left", 680, 100,
            "top", 200, 100,
          );
          addTextPopup(
            this.gameRefs,
            tween.first,
            () => {
              return this.logTextPool.newText(textPos, JSON.stringify(entry.action));
            },
            tween => {
              tween.to({ y: textPos.yMin - 100 }, 1000);
            },
            "log",
          );
          tween.first.onStart.add(() => {
            // clear trigger log entries
            this.gameRefs.screens.execScreen.logTriggerPool.clear();
            // update state view
            const prev = fetchPrevEntry(this.log!, entryIndex, type);
            const prevS = prev === undefined ? prevState : prev.state;
            this.gameRefs.screens.execScreen.drawState(entry.state, prevS);
            this.gameRefs.screens.execScreen.drawStats(entry.state);
          });
        }
        return tween;
      },
    };
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
    const sprite = this.solTreePool.newSprite(x - 50, y, {}, { type: initialNodeType });

    const drawPosList = drawPositions(solInfo.solution.tree);
      drawPosList.forEach((drawPos) => {
        const type = JSON.stringify(drawPos.loc) === JSON.stringify(solInfo.loc) ? "node_full" : "node";
        const sprite = this.solTreePool.newSprite(x + drawPos.x * 50, y + drawPos.y * 50, {}, { type });
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
  }
}

function fetchPrevEntry(
  log: Log,
  index: number,
  type: "st" | "fr" | "en",
): LogEntry | undefined {
  if (index === 0) {
    switch (type) {
      case "st": return undefined;
      case "fr": return log.st[log.st.length - 1];
      case "en": return log.fr[log.fr.length - 1];
    }
  } else {
    return log[type][index - 1];
  }
  throw `should not happen -- ${index} ${type}`;
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
          console.log("CLEAR!");
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
            clickState.inputs.push(self.data.globalId);
            if (clickState.inputs.length === clickState.ability.inputs.length) {
              extendLevelSolution(gameRefs, clickState!);
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
  type: "hp" | "ch" | "res_anim",
};

function mkUnitResPool(
  gameRefs: GameRefs,
): Pool<UnitResData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
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
            clickState.inputs.push(self.data.globalId);
            if (clickState.inputs.length === clickState.ability.inputs.length) {
              extendLevelSolution(gameRefs, clickState!);
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

type AbilityData = {
  ability: Ability,
  index: number,
  globalId: GlobalId<"friendly" | "enemy">,
};

function mkAbilityPool(
  gameRefs: GameRefs,
): Pool<AbilityData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return `${self.data.ability.spriteId}.png`;
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 20, Phaser.Easing.Linear.None, false, 5);
        },
      ],
      callbacks: {
        click: (self) => {
          if (gameRefs.screens.execScreen.clickState === undefined) {
            gameRefs.screens.execScreen.clickState = {
              ability: self.data.ability,
              inputs: [],
              origin: self.data.globalId,
            }
            if (self.data.ability.inputs.length === 0) {
              extendLevelSolution(gameRefs, gameRefs.screens.execScreen.clickState!);
            }
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
        return `icon_a.png`;
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 20, Phaser.Easing.Linear.None, false, 5);
        },
      ],
      callbacks: {
        click: (self) => {
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
};

function mkLogActionPool(
  gameRefs: GameRefs,
): Pool<LogActionData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return `icon_b.png`;
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
          gameRefs.screens.execScreen.drawState(self.data.state);
          gameRefs.screens.execScreen.drawStats(self.data.state);
        },
        hoverOut: (self) => {
          gameRefs.screens.execScreen.drawState(gameRefs.screens.execScreen.state!);
          gameRefs.screens.execScreen.drawStats(gameRefs.screens.execScreen.state!);
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
        return `icon_a.png`;
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
          
        },
      },
    },
  );
}