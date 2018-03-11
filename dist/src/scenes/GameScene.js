const buttonMap = PS["Client.Main"].buttonMap();

class GameScene extends Phaser.Scene {
  constructor() {
    super({
      key: 'GameScene'
    });
  }

  preload() {
    this.load.image('logo', 'assets/logo.png');
  }

  create() {
    const buttonConfig = {
      key: 'logo',
      x: buttonMap['1'].xRight,
      y: buttonMap['1'].xLeft

    };
    var button = this.add.sprite(buttonMap['1'].xLeft, buttonMap['1'].yTop, 'logo');
    console.log(button.texture);
    // workaround for setting width/height
    const origWidth = button.texture.source[0].width;
    const origHeight = button.texture.source[0].height;
    const wantedWidth = buttonMap['1'].xRight - buttonMap['1'].xLeft;
    const wantedHeight = buttonMap['1'].yBot - buttonMap['1'].yTop;

    button.scaleX = wantedWidth / origWidth;
    button.scaleY = wantedHeight / origHeight;
  }
};

export default GameScene;
