import { GameRefs } from "../../states/game";
import { TextPool } from "../../phaser/textpool";
import { enUnitMap } from "../../../shared/data/units/enemy";
import { frUnitMap } from "../../../shared/data/units/friendly";
import { createPosition } from "../../util/position";

export class CodexScreen {
  pageTextPool: TextPool

  page: { tag: "CardId", cardId: string } | undefined

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.pageTextPool = new TextPool(gameRefs.game);
  }

  drawPage() {
    this.redrawPage();
    // play anims
  }

  redrawPage() {
    this.pageTextPool.clear();

    if (this.page !== undefined) {
      switch (this.page.tag) {
        case "CardId": {
          this.drawCardIdPage(this.page.cardId);
        }
      }
    }
  }

  drawCardIdPage(
    cardId: string,
  ) {
    if (enUnitMap[cardId] !== undefined) {

    } else if (frUnitMap[cardId] !== undefined) {
      const unit = frUnitMap[cardId];
      const pos = createPosition(
        "left", 400, 200,
        "top", 200, 400,
      );
      this.pageTextPool.newText(pos, `CardID: ${cardId}`);
      unit.abilities.forEach((ability, abilityIndex) => {
        const pos = createPosition(
          "left", 400, 200,
          "top", 200 + 400 * abilityIndex, 400,
        );
        this.pageTextPool.newText(pos, `Ability: ${JSON.stringify(ability, undefined, 1)}`);
      });
    }
  }

  setVisibility(
    visibility: boolean
  ) {
    this.pageTextPool.setVisiblity(visibility);
  }

}
