import { GameRefs } from "../states/game";
import { TargetType } from "../../shared/game/entityId";
import { config } from "../config";
import { frUnitMap } from "../../shared/data/units/friendly";

export function drawHoverCard(
  game: Phaser.Game,
  gameRefs: GameRefs,
  type: TargetType,
  id: string,
  x: number,
  y: number,
) {
  const hoverCard: Phaser.Sprite = gameRefs.hoverViewPool.getFirstExists(false, true, x, y, "bg_hover");

  const btnString = `${frUnitMap[id].hp} HP`;
  const btnText = game.add.text(
    x, y, btnString, {
      fill: "#FF0000",
      fontSize: 75,
      boundsAlignH: "center",
      boundsAlignV: "middle",
    }
  );
  btnText.setTextBounds(0, 0, config.hoverCardWidth, 100);

  hoverCard.events.onKilled.removeAll();
  hoverCard.events.onKilled.add(() => {
    btnText.destroy();
  });
}