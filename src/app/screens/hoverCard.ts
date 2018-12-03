import { GameRefs } from "../states/game";
import { TargetType } from "../../shared/game/entityId";
import { config } from "../config";
import { frUnitMap } from "../../shared/data/units/friendly";
import { FrUnit } from "../../shared/game/unit";
import { abilityText } from "../../shared/game/ability";

export type HoverScreenData = {
  hoverViewPool: Phaser.Group,
  hoverAbilityPool: Phaser.Group,
}

export function clearHoverCard(
  gameRefs: GameRefs,
) {
  gameRefs.hoverScreenData.hoverViewPool.killAll();
  gameRefs.hoverScreenData.hoverAbilityPool.killAll();
}

export function drawHoverCardFriendly(
  game: Phaser.Game,
  gameRefs: GameRefs,
  id: string,
  x: number,
  y: number,
) {
  const unit: FrUnit = frUnitMap[id];

  const hoverCard: Phaser.Sprite = gameRefs.hoverScreenData.hoverViewPool.getFirstExists(false, true, x, y, "bg_hover");

  const texts: Phaser.Text[] = [];

  const hpString = `${frUnitMap[id].hp} HP`;
  const hpText = game.add.text(
    x, y, hpString, {
      fill: "#FF0000",
      fontSize: 75,
      boundsAlignH: "center",
      boundsAlignV: "middle",
    }
  );
  hpText.setTextBounds(0, 0, config.hoverCardWidth, 100);
  texts.push(hpText);

  const chString = `${frUnitMap[id].charges} CH`;
  const chText = game.add.text(
    x, y + 100, chString, {
      fill: "#FF0000",
      fontSize: 75,
      boundsAlignH: "center",
      boundsAlignV: "middle",
    }
  );
  chText.setTextBounds(0, 0, config.hoverCardWidth, 100);
  texts.push(chText);

  unit.abilities.forEach((ability, abilityIndex) => {
    const hoverAbility: Phaser.Sprite = gameRefs.hoverScreenData.hoverAbilityPool.getFirstExists(false, true, x + 25, y + 25 + 200 * (abilityIndex + 1), ability.spriteId);

    const abString = abilityText(ability);
    const abText = game.add.text(
      x + 200, y + 200 * (abilityIndex + 1), abString, {
        fill: "#FF0000",
        fontSize: 50,
        boundsAlignH: "center",
        boundsAlignV: "middle",
      }
    );
    abText.setTextBounds(0, 0, config.hoverCardWidth - 200, 100);
    texts.push(abText);
  });

  hoverCard.events.onKilled.removeAll();
  hoverCard.events.onKilled.add(() => {
    texts.forEach(x => x.destroy());
  });
}