import { Position } from "../util/position";

export class TextPool {
  texts: Phaser.Text[] = []
  game: Phaser.Game

  constructor(
    game: Phaser.Game,
  ) {
    this.game = game
  }

  public newText(
    pos: Position,
    textContent: string,
  ): Phaser.Text {
    const text = this.game.add.text(
      pos.xMin, pos.yMin, textContent, {
        fill: "#000000",
        fontSize: 70,
        boundsAlignH: "center",
        boundsAlignV: "middle",
      }
    );
    text.setTextBounds(0, 0, pos.xMax - pos.xMin, pos.yMax - pos.yMin);
    this.texts.push(text);
    return text;
  }

  public clear() {
    this.texts.forEach(x => x.kill());
  }

  public setVisiblity(
    visibility: boolean,
   ) {
     this.texts.forEach(x => x.visible = visibility);
   }
}