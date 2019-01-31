import { Pool, mkButtonPool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { createPosition, relativeTo } from "../../util/position";
import { addText } from "../../phaser/datasprite";
import { GameState, filteredEn, filteredFr, FrStUnit, EnStUnit } from "../../../shared/game/state";
import { Log } from "../../../shared/game/log";
import { cardMap } from "../../../app/data/cardMap";
import { TextPool } from "../../phaser/textpool";
import { getUnit, GlobalId } from "../../../shared/game/entityId";
import { hoverUnit, clearHover, clickUnit } from "./events";
import { Ability } from "src/shared/game/ability";

type UnitSelection = GlobalId<"friendly" | "enemy">;

export class ExecScreen {
  clearBtnPool: Pool<{}, "neutral" | "hover" | "down">
  unitPool: Pool<UnitData, {}>
  abilityPool: Pool<AbilityData, {}>
  textPool: TextPool

  state: GameState | undefined
  hoveredUnit: UnitSelection | undefined
  selectedUnit: UnitSelection | undefined
  clickstate: undefined

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.clearBtnPool = mkClearBtnPool(gameRefs);
    this.unitPool = mkUnitPool(gameRefs);
    this.textPool = new TextPool(gameRefs.game);
    this.abilityPool = mkAbilityPool(gameRefs);
  }

  reset() {
    this.state = undefined;
    this.hoveredUnit = undefined;
    this.selectedUnit = undefined;
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
  ) {
    const state = this.state!;
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
          "left", 500 + 200 * unitIndex, 150,
          "top", 300, 300,
        );
        const unitSprite = this.unitPool.newSprite(unitPos.xMin, unitPos.yMin, {},
          { cardId: unit.cardId,
            globalId: new GlobalId(unit.id, "friendly"),
          });
        //mkUnitPool(game, gameRefs, unitPos, unit.cardId, unitIndex, "friendly");

        // HP
        /*const unitHpPos = relativeTo(unitPos,
          "below", 50,
          150, 50,
        );
        unitHpPos.xMax = unitHpPos.xMin + (unitHpPos.xMax - unitHpPos.xMin) * (unit.hp / unit.maxHp);
        createUnitResource(game, gameRefs, unitHpPos, "hp");

        // CH
        const unitChPos = relativeTo(unitPos,
          "below", 150,
          150, 50,
        );
        unitChPos.xMax = unitChPos.xMin + (unitChPos.xMax - unitChPos.xMin) * (unit.charges / unit.maxCharges);
        createUnitResource(game, gameRefs, unitChPos, "ch");

        // TH
        let i = 0;
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
          "left", 1400 + 200 * unitIndex, 150,
          "top", 300, 300,
        );
        const unitSprite = this.unitPool.newSprite(unitPos.xMin, unitPos.yMin, {},
          { cardId: unit.cardId,
            globalId: new GlobalId(unit.id, "enemy"),
          });
      }
    });
  }

  drawStats(
  ) {
    this.clearBtnPool.clear();
    this.abilityPool.clear();
    this.textPool.clear();

    const textPos = createPosition(
      "left", 600, 150,
      "bot", 200, 300,
    );
    this.textPool.newText(textPos, "Skills");

    const showUnit = this.hoveredUnit !== undefined ? this.hoveredUnit : this.selectedUnit;
    if (showUnit !== undefined) {
      const unit = getUnit(showUnit, this.state!);
      if (unit !== undefined) {
        const pos1 = createPosition(
          "left", 600, 150,
          "bot", 100, 300,
        );
        this.textPool.newText(pos1, `${unit.hp} / ${unit.maxHp}`);

        if (showUnit.type === "friendly") {
          const frUnit = <FrStUnit>unit;
          frUnit.abilities.forEach((ability, abilityIndex) => {
            const abPos = createPosition(
              "left", 600 + 200 * abilityIndex, 150,
              "bot", 50, 150,
            );
            this.abilityPool.newSprite(abPos.xMin, abPos.yMin, {}, { ability, index: abilityIndex });
          });
        } else if (showUnit.type === "enemy") {
          const enUnit = <EnStUnit>unit;
        }
      }
    }
  }

  setVisibility(
    visibility: boolean,
  ) {
    this.clearBtnPool.visible = visibility;
    this.unitPool.visible = visibility;
    this.textPool.setVisiblity(visibility);
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
          clickUnit(gameRefs, self.data.globalId);
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

function mkResPool(
  gameRefs: GameRefs,
): Pool<{}, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return "";
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 20, Phaser.Easing.Linear.None, false, 5);
        },
      ],
      callbacks: {},
    },
  );
}

type AbilityData = {
  ability: Ability,
  index: number,
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
          
        },
      },
    },
  );
}