import { GameRefs } from "../../states/game";
import { TextPool } from "../../phaser/textpool";
import { enUnitMap, EnUnitId } from "../../../shared/data/units/enemy";
import { frUnitMap, FrUnitId } from "../../../shared/data/units/friendly";
import { createPosition } from "../../util/position";
import { Ability } from "../../../shared/game/ability";
import { GlobalId } from "../../../shared/game/entityId";
import { Pool } from "../../phaser/pool";
import { Unit } from "../../../shared/game/unit";
import { intentDescription } from "../../util/intentDesc";
import { Intent } from "src/shared/game/intent";

export type CodexTypes
  = { tag: "FrCardId", cardId: FrUnitId }
  | { tag: "EnCardId", cardId: EnUnitId }

export class CodexScreen {
  pageTextPool: TextPool
  abilityPool: Pool<AbilityData, {}>
  abilityDescPool: Pool<AbilityDescData, {}>

  page: CodexTypes | undefined
  showAbilityIndex: number | undefined

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.pageTextPool = new TextPool(gameRefs.game);
    this.abilityPool = mkAbilityPool(gameRefs);
    this.abilityDescPool = mkAbilityDescPool(gameRefs);
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
          this.drawEnCardIdPage(this.page.cardId);
          break;
        }
      }
    }
  }

  drawFrCardIdPage(
    cardId: FrUnitId,
  ) {
    if (frUnitMap[cardId] !== undefined) {
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
      
      unit.abilities.forEach(({ intent, spriteId, inputs }, abilityIndex) => {
        const pos = createPosition(
          "left", 550 + 125 * abilityIndex, 100,
          "top", 350, 100,
        );
        this.abilityPool.newSprite(pos.xMin, pos.yMin, {}, { intent, spriteId, index: abilityIndex });
      });
    } else {
      throw `wrong card id: ${cardId}`;
    }
  }

  drawEnCardIdPage(
    cardId: EnUnitId,
  ) {
    if (enUnitMap[cardId] !== undefined) {
      const unit = enUnitMap[cardId];
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
      
      unit.ai.forEach(({ intent, spriteId, outs }, aiIndex) => {
        const pos = createPosition(
          "left", 550 + 125 * aiIndex, 100,
          "top", 350, 100,
        );
        this.abilityPool.newSprite(pos.xMin, pos.yMin, {}, { intent, spriteId, index: aiIndex });
      });
    } else {
      throw `wrong card id: ${cardId}`;
    }
  }

  drawAbility(
  ) {
    this.abilityDescPool.clear();

    if (this.page !== undefined) {
      if (this.page.tag === "FrCardId") {
        const unit = frUnitMap[this.page.cardId];
        
        unit.abilities.forEach((ability, abilityIndex) => {
          if (this.showAbilityIndex !== undefined && this.showAbilityIndex === abilityIndex) {
            let y = 0;
            let xOffset = 0;
            const desc = intentDescription(ability.intent);
            desc.forEach((descSym, descIndex) => {
              const explPos = createPosition(
                "left", 150 + 80 * (descIndex - xOffset), 80,
                "bot", 250 - y * 80, 80,
              );
              switch (descSym.tag) {
                case "DescSeparator": {
                  y += 1;
                  xOffset = descIndex + 1;
                  break;
                }
                case "DescSymbol": {
                  this.abilityDescPool.newSprite(explPos.xMin, explPos.yMin, {}, { sprite: descSym.sym });
                  break;
                }
              }
            });
          }
        });
      } else if (this.page.tag === "EnCardId") {
        const unit = enUnitMap[this.page.cardId];
        
        unit.ai.forEach((ai, abilityIndex) => {
          if (this.showAbilityIndex !== undefined && this.showAbilityIndex === abilityIndex) {
            let y = 0;
            let xOffset = 0;
            const desc = intentDescription(ai.intent);
            desc.forEach((descSym, descIndex) => {
              const explPos = createPosition(
                "left", 150 + 80 * (descIndex - xOffset), 80,
                "bot", 250 - y * 80, 80,
              );
              switch (descSym.tag) {
                case "DescSeparator": {
                  y += 1;
                  xOffset = descIndex + 1;
                  break;
                }
                case "DescSymbol": {
                  this.abilityDescPool.newSprite(explPos.xMin, explPos.yMin, {}, { sprite: descSym.sym });
                  break;
                }
              }
            });
          }
        });
      }
    }
  }

  setVisibility(
    visibility: boolean
  ) {
    this.pageTextPool.setVisiblity(visibility);
  }

}

type AbilityData = {
  intent: Intent,
  spriteId: string,
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
        return `${self.data.spriteId}.png`;
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
          gameRefs.screens.codexScreen.showAbilityIndex = self.data.index;
          gameRefs.screens.codexScreen.drawAbility();
        },
        hoverOut: (self) => {
          gameRefs.screens.codexScreen.showAbilityIndex = undefined;
          gameRefs.screens.codexScreen.drawAbility();
        },
      },
    },
  );
}

type AbilityDescData = {
  sprite: string,
};


function mkAbilityDescPool(
  gameRefs: GameRefs,
): Pool<AbilityDescData, {}> {
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