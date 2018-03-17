import Phaser from 'phaser';

import PS from 'js/purs.bundle.js';

var gfx;

var downLocX;
var downLocY;
var oldCamX;
var oldCamY;

var zoom = 1;

var boundsPoint;
var viewRect;
var gameWorld;
var bgGroup;

var gameX = 2500;
var gameY = 2000;

var clickState = 0;
var click0;

var lines = [];

var connections = PS.initCxns;
var startNodes = [0];
var validNodes = [];

export default class extends Phaser.State {
  init() { }
  preload() {
    this.game.time.advancedTiming = true;
  }

  create() {
    gfx = this.game.add.graphics(0, 0);

    this.game.world.setBounds(-1000, -1000, 2000, 2000);

    boundsPoint = new Phaser.Point(0, 0);
    viewRect = new Phaser.Rectangle(0, 0, this.game.width, this.game.height);

    gameWorld = this.game.add.group();
    gameWorld.position.setTo(this.game.world.centerX, this.game.world.centerY);

    bgGroup = this.game.add.group(gameWorld);

    const startSize = 15;
    var startBulb = this.game.add.graphics(0, 0, bgGroup);
    startBulb.beginFill(0xFFFFFF);
    startBulb.drawRect(startSize * -0.5, startSize * -0.5, startSize, startSize);
    startBulb.endFill();
    startBulb.inputEnabled = true;
    startBulb.events.onInputDown.add(this.bulbClick(0), this);

    for (var i = 1; i <= 1000; i++) {
      var size = this.game.rnd.integerInRange(7, 9);
      var sqr = this.game.add.graphics(this.game.rnd.integerInRange(-gameX, gameX), this.game.rnd.integerInRange(-gameY, gameY), bgGroup);
      var clr = this.game.rnd.integerInRange(1,10);
      if (clr === 1) {
        sqr.beginFill(0xFF0000);
      } else {
        sqr.beginFill(0x666666);
      }
      sqr.drawRect(size * -0.5, size * -0.5, size, size);
      sqr.endFill();
      sqr.inputEnabled = true;
      sqr.events.onInputDown.add(this.bulbClick(i), this);
    }

    this.game.camera.x = (this.game.width * -0.5);
    this.game.camera.y = (this.game.height * -0.5);

    this.game.input.onDown.add(this.down, this);
    this.game.input.onUp.add(this.up, this);
  }

  render() {
  }

  update () {
    if (this.game.input.activePointer.isDown) {
      this.hold(this.game.input.activePointer);
    }

    if (this.game.input.keyboard.isDown(Phaser.Keyboard.Q)) {
      zoom += 0.05;
    }
    if (this.game.input.keyboard.isDown(Phaser.Keyboard.A)) {
      zoom -= 0.05;
    }
    zoom = Phaser.Math.clamp(zoom, 0.3, 2);

    gameWorld.scale.set(zoom);

    bgGroup.forEachExists(function(circ) {
      boundsPoint.setTo(
        ((circ.x - gameWorld.pivot.x) * gameWorld.scale.x) + (game.width * 0.5),
        ((circ.y - gameWorld.pivot.y) * gameWorld.scale.y) + (game.height * 0.5)
      );
      if (Phaser.Rectangle.containsPoint(viewRect, boundsPoint)) {
        circ.visible = true;
      } else {
      }
    });

  }

  down(x) {
    this.downLocX = this.game.input.activePointer.x;
    this.downLocY = this.game.input.activePointer.y;
    this.oldCamX = gameWorld.pivot.x;
    this.oldCamY = gameWorld.pivot.y;
  }

  up(x) {
  }

  hold(x) {
    // TODO: check if vars are undefined?
    const currentX = this.game.input.activePointer.x;
    const currentY = this.game.input.activePointer.y;

    const diffX = currentX - this.downLocX;
    const diffY = currentY - this.downLocY;

    //this.game.camera.x = this.oldCamX - diffX;
    //this.game.camera.y = this.oldCamY - diffY;
    gameWorld.pivot.x = Phaser.Math.clamp(this.oldCamX - diffX * (0.5 / zoom), -gameX, gameX);
    gameWorld.pivot.y = Phaser.Math.clamp(this.oldCamY - diffY * (0.5 / zoom), -gameY, gameY);
  }

  bulbClick(i) {
    return function(bulb) {
      console.log("clicked on " + i);

      if (clickState == 0) {
        clickState = 1;
        click0 = {
          x: bulb.x,
          y: bulb.y,
          id: i
        };
        console.log("x " + click0.x + " y " + click0.y);
      } else if (clickState == 1) {
        const click1 = {
          x: bulb.x,
          y: bulb.y,
          id: i
        };
        clickState = 0;

        const fromDist = Phaser.Point.distance(new Phaser.Point(0, 0), new Phaser.Point(click0.x, click0.y));
        const toDist = Phaser.Point.distance(new Phaser.Point(0, 0), new Phaser.Point(click1.x, click1.y));

        var tup;
        if (fromDist > toDist) {
          tup = { closest: click1, furthest: click0 };
        } else {
          tup = { closest: click0, furthest: click1 };
        }

        if (Array.includes(validNodes, tup.furthest.id)) {
          console.log("rejected");
        } else {
          console.log("creating line");

          if (Array.includes(startNodes, tup.furthest.id)) {
            console.log("end point is start node");
            const index = startNodes.indexOf(tup.furthest.id);
            startNodes.splice(index, 1);
            validNodes.push(tup.furthest.id);
          }

          if (! Array.includes(startNodes, tup.closest.id)) {
            console.log("new start node");
            startNodes.push(tup.closest.id);
          }

          // draw line
          var line = this.game.add.graphics(0, 0, bgGroup);
          line.lineStyle(5, 0x000000);
          line.moveTo(click0.x, click0.y);
          line.lineTo(click1.x, click1.y);
          line.endFill();

          // add to connections
          connections = PS.addLink(tup)(connections);

          // verify costs/usage
          const verifyResult = PS.verifyLinksJs(connections);
          console.log(verifyResult);

          // add to furthest valid nodes
          // closest should already be a valid or start node
          validNodes.push(tup.furthest.id);
        }
      }
    };
  }
}
