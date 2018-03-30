import * as Phaser from 'phaser-ce';
import * as WebFont from 'webfontloader';
import config from 'src/config';

export default class extends Phaser.State {
  fontsReady = false;

  init() {
    this.stage.backgroundColor = '#EDEEC9';
    this.fontsLoaded = this.fontsLoaded.bind(this);
  }

  preload() {
    if (config.webfonts.length) {
      WebFont.load({
        google: {
          families: config.webfonts
        },
        active: this.fontsLoaded
      });
    }

    let text: Phaser.Text = this.add.text(this.world.centerX, this.world.centerY, 'loading fonts', { font: '16px Arial', fill: '#dddddd', align: 'center' });
    text.anchor.setTo(0.5, 0.5);

    this.load.image('loaderBg', './assets/images/loader-bg.png');
    this.load.image('loaderBar', './assets/images/loader-bar.png');
  }

  render() {
    if (config.webfonts.length && this.fontsReady) {
      this.state.start('Splash');
    }
    if (!config.webfonts.length) {
      this.state.start('Splash');
    }
  }

  fontsLoaded() {
    this.fontsReady = true;
  }
}
