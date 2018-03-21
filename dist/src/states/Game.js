import Phaser from 'phaser';

import PS from 'js/purs.bundle.js';

const gameUiMap = PS.gameUiMap();

var mouseOverMenu;
var mouseOverText;

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

var bulbId = 0;

// game resources
var resourceText;

var growth = 100;

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

    this.mkBulb({ size: 15, group: bgGroup, x: 0, y: 0, color: 0xFFFFFF });

    const levels1 = 2;
    const quadrants1 = 3;
    const color1 = 0x666666;
    for (var level = 0; level <= levels1; level++) {
      for (var quadrant = 0; quadrant <= quadrants1; quadrant++) {
        for (var i = 0; i <= 1; i++) {
          const size = this.game.rnd.integerInRange(7, 9);
          const pos = this.rndXY({ ampMin: 50, ampMax: 250, level: level, levels: levels1, quadrant: quadrant });
          this.mkBulb({ size: size, group: bgGroup, x: pos.x, y: pos.y, color: color1 });
        }
      }
    }

    const levels2 = 3;
    const quadrants2 = 3;
    const color2 = 0xAAAAAA;
    for (var level = 0; level <= levels2; level++) {
      for (var quadrant = 0; quadrant <= quadrants2; quadrant++) {
        for (var i = 0; i <= 1; i++) {
          const size = this.game.rnd.integerInRange(9, 11);
          const pos = this.rndXY({ ampMin: 200, ampMax: 700, level: level, levels: levels2, quadrant: quadrant });
          // TODO create unique id
          this.mkBulb({ size: size, group: bgGroup, x: pos.x, y: pos.y, color: color2 });
        }
      }
    }

    this.game.camera.x = (this.game.width * -0.5);
    this.game.camera.y = (this.game.height * -0.5);

    this.game.input.onDown.add(this.down, this);
    this.game.input.onUp.add(this.up, this);

    // bottom right, mouseover menu

//    mouseOverMenu = this.add.sprite(gameUiMap['1'].xLeft, gameUiMap['1'].yTop, 'bulb');
//    console.log(gameUiMap['1']);
//    mouseOverMenu.width = gameUiMap['1'].xRight - gameUiMap['1'].xLeft;
//    mouseOverMenu.height = gameUiMap['1'].yBot - gameUiMap['1'].yTop;
    mouseOverMenu = this.add.sprite(300, 200, 'bulb');
    console.log(gameUiMap['1']);
    mouseOverMenu.width = 100;
    mouseOverMenu.height = 100;

    mouseOverMenu.visible = false;

    mouseOverText = this.add.text(300, 200, '', {
      font: '20px Indie Flower',
      fill: '#77BFA3'
    });

    mouseOverText.visible = false;

    mouseOverText.padding.set(10, 16);
    mouseOverText.anchor.setTo(0, 0);

    // bottom, resource values
    resourceText = this.add.text(-375, 275, 'growth: 100', {
      font: '20px Indie Flower',
      fill: '#77BFA3'
    });

  }

  rndXY(o) {
    const ampMin = o.ampMin;
    const ampMax = o.ampMax;
    const level = o.level;
    const levels = o.levels;
    const quadrant = o.quadrant;

    const levelAmp = (ampMax - ampMin) / (levels + 1);

    var φ = Phaser.Math.degToRad(this.game.rnd.integerInRange(quadrant * 90, (quadrant + 1) * 90));
    var r = this.game.rnd.integerInRange(ampMin + (level * levelAmp), ampMin + ((level + 1) * levelAmp));
    var x = r * Math.cos(φ);
    var y = r * Math.sin(φ);

    return { x: x, y: y };
  }

  mkBulb(o) {
    const size = o.size;
    const group = o.group;
    const color = o.color;
    const x = o.x;
    const y = o.y;

    const id = bulbId;
    bulbId += 1;

    var bulb = this.game.add.graphics(x, y, group);
    bulb.beginFill(color);
    bulb.drawRect(size * -0.5, size * -0.5, size, size);
    bulb.endFill();
    bulb.inputEnabled = true;
    bulb.events.onInputDown.add(this.bulbClick(id), this);
    bulb.events.onInputOver.add(this.bulbOver(id), this);
    //bulb.events.onInputOut.add(this.bulbOut(id), this);

    return bulb;
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

        const fromDist = Phaser.Math.distance(0, 0, click0.x, click0.y);
        const toDist = Phaser.Math.distance(0, 0, click1.x, click1.y);

        var tup;
        if (fromDist > toDist) {
          tup = { closest: click1, furthest: click0 };
        } else {
          tup = { closest: click0, furthest: click1 };
        }
        tup.distance = Phaser.Point.distance(new Phaser.Point(click0.x, click0.y), new Phaser.Point(click1.x, click1.y)) / 20;
        console.log("distance: " + tup.distance);

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
          const verifyResult = PS.calcResource(connections);
          console.log(verifyResult);

          // add to furthest valid nodes
          // closest should already be a valid or start node
          validNodes.push(tup.furthest.id);
        }
      }
    };
  }

  bulbOver(i) {
    return function (bulb, pointer) {
      console.log("over " + i);
      mouseOverMenu.visible = true;
      mouseOverText.setText('ID: ' + i);
      mouseOverText.visible = true;
    };
  }

  bulbOut(i) {
    return function (bulb, pointer) {
      console.log("out " + i);
      //mouseOverMenu.visible = false;
      //mouseOverText.text = '';
      //mouseOverText.visible = false;
    };
  }

  setGrowth(x) {
    growth = x;
    resourceText.setText('growth: ' + x);
  }
}
