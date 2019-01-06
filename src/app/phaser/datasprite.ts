export interface DataSprite<Data> extends Phaser.Sprite {
  data: Data
  props?: {
    // init status:
    // a sprite is initialized once, which sets this flag to prevent reinitialization
    init: boolean,
    // selecting status:
    // a sprite is being selected when it has received a pointer down event
    // but the pointer has not been lifted up yet
    selecting: boolean,
  }
}