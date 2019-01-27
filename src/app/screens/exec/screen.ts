import { Pool, mkButtonPool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { createPosition, relativeTo } from "../../util/position";
import { addText } from "../../phaser/datasprite";
import { GameState, filteredEn, filteredFr } from "../../../shared/game/state";
import { Log } from "../../../shared/game/log";
import { cardMap } from "../../../app/data/cardMap";

export class ExecScreen {
  clearBtnPool: Pool<{}, "neutral" | "hover" | "down">
  unitPool: Pool<UnitData, {}>

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.clearBtnPool = mkClearBtnPool(gameRefs);
    this.unitPool = mkUnitPool(gameRefs);
  }

  drawClearBtn(
  ) {
    this.redrawExecStartBtn();
    this.clearBtnPool.playIntroAnimations();
  }

  redrawExecStartBtn(
  ) {
    this.clearBtnPool.killAll();

    const pos = createPosition(
      "right", 400, 400,
      "bot", 300, 200,
    );
    const sprite = this.clearBtnPool.newSprite(pos.xMin, pos.yMin, "neutral", {});
    addText(this.gameRefs, sprite, pos, "Clear Solution", "#000000", 40);
  }

  drawFriendlyUnits(
    state: GameState,
  ) {
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
        console.log(`drawing: ${unit.cardId}`);
        const unitPos = createPosition(
          "left", 500 + 200 * unitIndex, 150,
          "top", 300, 300,
        );
        const unitSprite = this.unitPool.newSprite(unitPos.xMin, unitPos.yMin, {}, { cardId: unit.cardId });
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
  }

  setVisibility(
    visibility: boolean,
  ) {
    this.clearBtnPool.visible = visibility;
    this.unitPool.visible = visibility;
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
      callbacks: {},
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