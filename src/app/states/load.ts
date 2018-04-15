export default class Load extends Phaser.State {
  public create(): void {
    const loadingText: Phaser.Text = this.game.add.text(0, 0, "loading...", {
      fill: "#D3D3D3",
      boundsAlignH: "center",
      boundsAlignV: "middle"
    });

    loadingText.setTextBounds(200, 200, 200, 100);

    this.startMenu();
  }

  private startMenu(): void {
    this.game.state.start("menu");
  }
}
