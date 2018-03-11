import 'phaser';
import BootScene from 'src/scenes/BootScene.js';
import GameScene from 'src/scenes/GameScene.js';

const config =
 { type: Phaser.AUTO
 , width: 800
 , height: 600
 , scene:
   [ BootScene
   , GameScene
   ]
};

const game = new Phaser.Game(config);
