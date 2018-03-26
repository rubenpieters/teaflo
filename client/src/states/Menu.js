import Phaser from 'phaser';
import Mushroom from 'src/sprites/Mushroom';

import PS from 'js/purs.bundle.js';

const buttonMap = PS.buttonMap();

export default class extends Phaser.State {
  init() { }
  preload() { }

  create() {
    console.log(buttonMap);

    const bannerText = 'TeaFlo ';
    let banner = this.add.text(buttonMap['1'].xLeft, buttonMap['1'].yTop, bannerText, {
      font: '40px Indie Flower',
      fill: '#77BFA3',
      smoothed: false
    });

    banner.width = buttonMap['1'].xRight - buttonMap['1'].xLeft;
    banner.height = buttonMap['1'].yBot - buttonMap['1'].yTop;

    banner.padding.set(10, 16);
    banner.anchor.setTo(0, 0);

    const xWidth2 = buttonMap['2'].xRight - buttonMap['2'].xLeft;
    const yHeight2 = buttonMap['2'].yBot - buttonMap['2'].yTop;
    const xCenter2 = buttonMap['2'].xLeft + (xWidth2 / 2);
    const yCenter2 = buttonMap['2'].yTop + (yHeight2 / 2);

    this.mushroom = new Mushroom({
      game: this.game,
      x: xCenter2,
      y: yCenter2,
      asset: 'mushroom'
    });

    this.mushroom.width = xWidth2;
    this.mushroom.height = yHeight2;

    this.mushroom.inputEnabled = true;
    this.mushroom.events.onInputDown.add(this.startGame, this);

    this.game.add.existing(this.mushroom);
  }

  render() {
  }

  startGame() {
    this.state.start('Game');
  }
}

