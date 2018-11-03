import { fromBottom, fromLeft } from "../config";

export const actNumberMap: { [key: number]: string } = {
  0: "1",
  1: "2",
  2: "...",
};

export function actSelect_Main(
  game: Phaser.Game,
  group: Phaser.Group
) {
  let allButtons: Phaser.Sprite[] = [];

  let i = 0;
  for (const actNumber in actNumberMap) {
    let btnString = actNumberMap[actNumber];

    const spriteSizeX = 200;
    const spriteSizeY = 200;
    const xMin = fromLeft(100 + spriteSizeX * i);
    const xMax = fromLeft(100 + spriteSizeX * i) + spriteSizeX;
    const yMin = fromBottom(spriteSizeY);
    const yMax = fromBottom(0);

    const btnSprite = game.add.sprite(
      xMin,
      yMin,
      "level_btn_neutral",
      undefined,
      group
    );
    btnSprite.inputEnabled = true;
    btnSprite.events.onInputUp.add(() => {
      if (! btnSprite.data.selected) {
        // Change this button to selected
        allButtons.map(x => {
          if (x.data.selected) {
            x.loadTexture("level_btn_neutral");
          }
          x.data.selected = false
        });
        btnSprite.loadTexture("level_btn_down");
        btnSprite.data.selected = true;
      }
    });
    btnSprite.events.onInputOver.add(() => {
      if (! btnSprite.data.selected) {
        btnSprite.loadTexture("level_btn_over");
      }
    });
    btnSprite.events.onInputOut.add(() => {
      if (! btnSprite.data.selected) {
        btnSprite.loadTexture("level_btn_neutral");
      }
    });

    const btnText = game.add.text(
      0, 0, btnString, {
        fill: "#FF0000",
        fontSize: 100,
        boundsAlignH: "center",
        boundsAlignV: "middle",
      }
    );
    btnText.setTextBounds(0, 0, xMax - xMin, yMax - yMin);
    btnSprite.addChild(btnText);

    allButtons.push(btnSprite);
    i += 1;
  }
}
