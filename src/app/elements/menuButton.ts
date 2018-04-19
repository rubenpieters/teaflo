export default class MenuButton {
  menuText: Phaser.Text

  constructor(game: Phaser.Game, x: number, y: number, width: number, height: number, text: string, font: string, fill: string) {
    this.menuText = game.add.text(x, y, text, {
      font: font,
      fill: fill,
      boundsAlignH: "center",
      boundsAlignV: "middle"
    });
    this.menuText.setTextBounds(x, y, width, height);
  }
}
