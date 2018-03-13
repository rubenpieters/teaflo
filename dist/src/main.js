import 'pixi';
import 'p2';
import Phaser from 'phaser';

import BootState from 'src/states/Boot';
import SplashState from 'src/states/Splash';
import MenuState from 'src/states/Menu';
import GameState from 'src/states/Game';

import config from 'src/config';

class Game extends Phaser.Game {
  constructor () {
    const docElement = document.documentElement;
    const width = 800;
    //docElement.clientWidth > config.gameWidth ? config.gameWidth : docElement.clientWidth;
    const height = 600;
    //docElement.clientHeight > config.gameHeight ? config.gameHeight : docElement.clientHeight;

    super(width, height, Phaser.CANVAS, 'content', null);

    this.state.add('Boot', BootState, false);
    this.state.add('Splash', SplashState, false);
    this.state.add('Menu', MenuState, false);
    this.state.add('Game', GameState, false);

    // with Cordova with need to wait that the device is ready so we will call the Boot state in another file
    if (!window.cordova) {
      this.state.start('Boot');
    }
  }
}

window.game = new Game();

if (window.cordova) {
  var app = {
    initialize: function () {
      document.addEventListener(
        'deviceready',
        this.onDeviceReady.bind(this),
        false
      );
    },

    // deviceready Event Handler
    //
    onDeviceReady: function () {
      this.receivedEvent('deviceready');

      // When the device is ready, start Phaser Boot state.
      window.game.state.start('Boot');
    },

    receivedEvent: function (id) {
      console.log('Received Event: ' + id);
    }
  };

  app.initialize();
}
