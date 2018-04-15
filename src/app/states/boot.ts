export default class Boot extends Phaser.State {
  public create(): void {
    this.game.state.start("load");
  }
}
