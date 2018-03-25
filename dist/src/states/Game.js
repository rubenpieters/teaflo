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
var validNodes = [0];

var bulbId = 0;

// game resources
var resourceText;

var resources = {
  growth: 100,
  white: 0,
  blue: 0,
  red: 0,
  green: 0,
  yellow: 0
};

var resourceMap = {};

resourceMap[0] = resources;


function nextId() {
  const v = bulbId;
  bulbId++;
  return v;
};

function integerInRange(g) {
  return function(x) {
    return function(y) {
      return function() {
        return g.rnd.integerInRange(x, y);
      };
    };
  };
};

export default class extends Phaser.State {
  init() { }
  preload() {
    this.game.time.advancedTiming = true;
  }

  create() {
    var board = PS.generateBoardJS({ nextId: nextId, integerInRange: integerInRange(this.game) })();
    console.log("board");
    console.log(board);


    gfx = this.game.add.graphics(0, 0);

    this.game.world.setBounds(-1000, -1000, 2000, 2000);

    boundsPoint = new Phaser.Point(0, 0);
    viewRect = new Phaser.Rectangle(0, 0, this.game.width, this.game.height);

    gameWorld = this.game.add.group();
    gameWorld.position.setTo(this.game.world.centerX, this.game.world.centerY);

    bgGroup = this.game.add.group(gameWorld);

    PS.drawBoardJS({ drawNode: this.drawNode(this) })(board)();

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

  drawNode(t) {
    return function(o) {
      return function() {
        const id = o.id;
        const x = o.x;
        const y = o.y;
        const nodeType = o.nodeType;
        const size = 8;

        const node = t.game.add.graphics(x, y, bgGroup);
        node.beginFill(PS.nodeColorJS(nodeType));
        node.drawRect(size * -0.5, size * -0.5, size, size);
        node.endFill();
        node.inputEnabled = true;

        node.events.onInputDown.add(t.bulbClick(id, nodeType), t);
        node.events.onInputOver.add(t.bulbOver(id, nodeType), t);
        //bulb.events.onInputOut.add(this.bulbOut(id, nodeType), this);

        return node;
      };
    };
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

  bulbClick(i, nodeType) {
    return function(bulb) {
      console.log("clicked on " + i);

      if (clickState == 0) {
        clickState = 1;
        click0 = {
          x: bulb.x,
          y: bulb.y,
          id: i,
          nodeType: nodeType
        };
        console.log("x " + click0.x + " y " + click0.y);
      } else if (clickState == 1) {
        const click1 = {
          x: bulb.x,
          y: bulb.y,
          id: i,
          nodeType: nodeType
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
        } else if (! Array.includes(validNodes, tup.closest.id)) {
          console.log("new start node, rejected");
        } else {
          console.log("creating line");

          const verifyResult = PS.verifyCost(resourceMap[tup.closest.id])(tup.furthest.nodeType);
          console.log(verifyResult);
          if (verifyResult.canBuy) {
            // draw line
            var line = this.game.add.graphics(0, 0, bgGroup);
            line.lineStyle(5, 0x000000);
            line.moveTo(click0.x, click0.y);
            line.lineTo(click1.x, click1.y);
            line.endFill();

            // add to connections
            connections = PS.addLink(tup)(connections);

            // add to furthest valid nodes
            // closest should already be a valid or start node
            validNodes.push(tup.furthest.id);

            // update resources
            resourceMap[tup.furthest.id] = verifyResult.newResources;
          }
        }
      }
    };
  }

  bulbOver(i, nodeType) {
    return function (bulb, pointer) {
      console.log("over " + i);
      mouseOverMenu.visible = true;
      const res = resourceMap[i];
      if (typeof res !== "undefined") {
        const resourceText = 'W: ' + res.white + 'B: ' + res.blue;
        mouseOverText.setText('ID: ' + i + '\n' + resourceText);
      } else {
        mouseOverText.setText('ID: ' + i);
      }
      mouseOverText.visible = true;
    };
  }

  bulbOut(i, nodeType) {
    return function (bulb, pointer) {
      console.log("out " + i);
      //mouseOverMenu.visible = false;
      //mouseOverText.text = '';
      //mouseOverText.visible = false;
    };
  }

  setGrowth(x) {
    resources.growth = x;
    resourceText.setText('growth: ' + x);
  }

}
