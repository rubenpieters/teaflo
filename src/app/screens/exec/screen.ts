import { Pool, mkButtonPool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { createPosition, relativeTo } from "../../util/position";
import { addText, DataSprite } from "../../phaser/datasprite";
import { GameState, filteredEn, filteredFr, FrStUnit, EnStUnit } from "../../../shared/game/state";
import { Log, LogEntry } from "../../../shared/game/log";
import { cardMap } from "../../../app/data/cardMap";
import { TextPool } from "../../phaser/textpool";
import { getUnit, GlobalId, UnitId, getStatus } from "../../../shared/game/entityId";
import { hoverUnit, clearHover, clickUnit, extendLevelSolution } from "./events";
import { Ability } from "../../../shared/game/ability";
import { triggerOrder, StTrigger } from "../../../shared/game/trigger";
import { Action } from "../../../shared/game/action";
import { chainSpriteCreation, createTween, addTextPopup } from "../../../app/phaser/animation";

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

  drawFriendlyUnits(
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
    this.logActionPool.clear(); 
    // on every log action:
    //   intro animation for log action icon
    //   do animation on state
    //   draw new state after that action
    this.logActionPool.clear();
    this.logTextPool.clear();
    let spriteFs: {
      create: () => DataSprite<any>,
      introTween: (sprite: DataSprite<any>) => { first: Phaser.Tween, last: Phaser.Tween } | undefined,
    }[];
    const stF = this.log!.st.map((entry, entryIndex) => {
      return {
        create: () => {
          const pos = createPosition(
            "left", 40 + entryIndex * 70, 150,
            "top", 40, 300,
          );
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
            );
            tween.first.onStart.add(() => {
              const prev = fetchPrevEntry(this.log!, entryIndex, "st");
              const prevS = prev === undefined ? prevState : prev.state;
              //console.log(`${JSON.stringify(prevState!.enUnits)}`);
              this.gameRefs.screens.execScreen.drawFriendlyUnits(entry.state, prevS);
              this.gameRefs.screens.execScreen.drawStats(entry.state);
            });
          }
          return tween;
        },
      };
    });
    const frF = this.log!.fr.map((entry, entryIndex) => {
      return {
        create: () => {
          const pos = createPosition(
            "left", 40 + entryIndex * 70, 150,
            "top", 150, 300,
          );
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
            );
            tween.first.onStart.add(() => {
              const prev = fetchPrevEntry(this.log!, entryIndex, "fr");
              const prevS = prev === undefined ? undefined : prev.state;
              //console.log(`${JSON.stringify(prevState!.enUnits)}`);
              this.gameRefs.screens.execScreen.drawFriendlyUnits(entry.state, prevS);
              this.gameRefs.screens.execScreen.drawStats(entry.state);
            });
          }
          return tween;
        },
      };
    });
    const enF = this.log!.en.map((entry, entryIndex) => {
      return {
        create: () => {
          const pos = createPosition(
            "left", 40 + entryIndex * 70, 150,
            "top", 240, 300,
          );
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
            );
            tween.first.onStart.add(() => {
              const prev = fetchPrevEntry(this.log!, entryIndex, "en");
              const prevS = prev === undefined ? undefined : prev.state;
              //console.log(`${JSON.stringify(prevState!.enUnits)}`);
              this.gameRefs.screens.execScreen.drawFriendlyUnits(entry.state, prevS);
              this.gameRefs.screens.execScreen.drawStats(entry.state);
            });
          }
          return tween;
        },
      };
    });
    spriteFs = stF.concat(frF).concat(enF);
    chainSpriteCreation(spriteFs, true);
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
    this.statsTextPool.setVisiblity(visibility);
    this.unitTextPool.setVisiblity(visibility);
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
        },
      ],
      callbacks: {
        click: (self) => {
        },
        hoverOver: (self) => {
          gameRefs.screens.execScreen.drawFriendlyUnits(self.data.state);
          gameRefs.screens.execScreen.drawStats(self.data.state);
        },
        hoverOut: (self) => {
          gameRefs.screens.execScreen.drawFriendlyUnits(gameRefs.screens.execScreen.state!);
          gameRefs.screens.execScreen.drawStats(gameRefs.screens.execScreen.state!);
        },
      },
    },
  );
}