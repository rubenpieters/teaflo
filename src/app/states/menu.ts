export default class Menu extends Phaser.State {
  public create(): void {
    const top1: Phaser.Text = this.add.text(0, 0, "Boards", {
      font: "60px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });
    top1.setTextBounds(0, 0, 250, 100);

    const top2: Phaser.Text = this.add.text(0, 0, "Play", {
      font: "60px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });
    top2.setTextBounds(250, 0, 250, 100);

  }
}
