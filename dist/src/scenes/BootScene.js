class BootScene extends Phaser.Scene {
  constructor() {
    super({ key: 'BootScene '});
  }

  preload() {
  }

  create() {
    this.scene.start('GameScene');
  }
}

export default BootScene;
