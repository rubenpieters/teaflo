import { GameRefs } from "../../states/game";
import { TextPool } from "../../phaser/textpool";
import { enUnitMap } from "../../../shared/data/units/enemy";
import { frUnitMap } from "../../../shared/data/units/friendly";
import { createPosition } from "../../util/position";
import { Ability } from "../../../shared/game/ability";
import { GlobalId } from "../../../shared/game/entityId";
import { Pool } from "../../phaser/pool";

export type CodexTypes
  = { tag: "FrCardId", cardId: string }
  | { tag: "EnCardId", cardId: string }

export class CodexScreen {
  pageTextPool: TextPool
  abilityPool: Pool<AbilityData, {}>

  page: CodexTypes | undefined

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.pageTextPool = new TextPool(gameRefs.game);
    this.abilityPool = mkAbilityPool(gameRefs);
  }

  reset() {

  }

  drawPage() {
    this.redrawPage();
    // play anims
  }

  redrawPage() {
    this.pageTextPool.clear();

    if (this.page !== undefined) {
      switch (this.page.tag) {
        case "FrCardId": {
          this.drawFrCardIdPage(this.page.cardId);
          break;
        }
        case "EnCardId": {
          //this.drawFrCardIdPage(this.page.cardId);
          break;
        }
      }
    }
  }

  drawFrCardIdPage(
    cardId: string,
  ) {
    if (enUnitMap[cardId] !== undefined) {
      throw `wrong card id: ${cardId}`;
    } else if (frUnitMap[cardId] !== undefined) {
      const unit = frUnitMap[cardId];
      /*const pos = createPosition(
        "left", 400, 200,
        "top", 50, 200,
      );
      this.pageTextPool.newText(pos, `${cardId}`);*/
      const pos1 = createPosition(
        "left", 550, 150,
        "top", 50, 150,
      );
      this.pageTextPool.newText(pos1, `${unit.hp} / ${unit.maxHp}`);
      const pos2 = createPosition(
        "left", 550, 150,
        "top", 200, 150,
      );
      this.pageTextPool.newText(pos2, `${unit.charges} / ${unit.maxCharges}`);
      
      unit.abilities.forEach((ability, abilityIndex) => {
        const pos = createPosition(
          "left", 550 + 125 * abilityIndex, 100,
          "top", 300, 100,
        );
        this.abilityPool.newSprite(pos.xMin, pos.yMin, {}, { ability, index: abilityIndex });
      });
    }
  }

  setVisibility(
    visibility: boolean
  ) {
    this.pageTextPool.setVisiblity(visibility);
  }

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
        hoverOver: (self) => {
          //gameRefs.screens.execScreen.showAbilityIndex = self.data.index;
          //gameRefs.screens.execScreen.drawStats(gameRefs.screens.execScreen.currentState());
        },
        hoverOut: (self) => {
          //gameRefs.screens.execScreen.showAbilityIndex = undefined;
          //gameRefs.screens.execScreen.drawStats(gameRefs.screens.execScreen.currentState());
        },
      },
    },
  );
}