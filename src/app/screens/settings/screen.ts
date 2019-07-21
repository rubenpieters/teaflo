import { GameRefs } from "../../states/game";
import { Pool, mkButtonPool } from "../../phaser/pool";
import { addText } from "../../phaser/datasprite";
import { saveSettings } from "../../data/settings";

export class SettingsScreen {
  btnPool: Pool<BtnData, "neutral" | "hover" | "down">

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.btnPool = mkBtnPool(this.gameRefs);
  }

  setVisibility(
    visibility: boolean
  ) {
  }

  drawBtn() {
    this.btnPool.clear();
    const btn = this.btnPool.newSprite(100, 100, "neutral", {});
    addText(this.gameRefs, btn, { xMin: 20, yMin: 20, xMax: 80, yMax: 80 }, JSON.stringify(this.gameRefs.settings.devMode), "0x000000", 10);
  }

}

type BtnData = {
}

function mkBtnPool(
  gameRefs: GameRefs,
): Pool<BtnData, "neutral" | "hover" | "down"> {
  return mkButtonPool(
    gameRefs,
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
          
        }
      ],
      callbacks: {
        click: (self) => {
          gameRefs.settings.devMode = ! gameRefs.settings.devMode;
          gameRefs.screens.settingsScreen.drawBtn();
          saveSettings(gameRefs);
        },
      },
    },
    self => { return false; }
  );
}