import Phaser from 'phaser';

import PS from 'js/purs.bundle.js';

const gameUiMap = PS.gameUiMap();

var socket;

var board;

var resourceMenu;
var totalText;
var branchText;

var effectMenu;
var effectMenuText;

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
var resources = {
  growth: 100,
  white: 0,
  blue: 0,
  red: 0,
  green: 0,
  yellow: 0,
  vp: 0
};

var resourceMap = {};

resourceMap[0] = resources;

var totals = resources;

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
  init(data) {
    let givenBoard = data.board;
    let givenSocket = data.socket;
    // set board
    if (typeof givenBoard === "undefined") {
      board = PS.generateBoardJS({ nextId: nextId, integerInRange: integerInRange(this.game) })();
    } else {
      board = givenBoard;
    }
    // set socket
    socket = givenSocket;
  }
  preload() {
    this.game.time.advancedTiming = true;
  }

  create() {
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

    // left - effects menu

    resourceMenu = this.game.add.graphics(gameUiMap['1'].xLeft - 400, gameUiMap['1'].yTop - 300);
    resourceMenu.beginFill(0x227744);
    resourceMenu.drawRect(0, 0, gameUiMap['1'].xWidth, gameUiMap['1'].yHeight);
    resourceMenu.endFill();

    totalText = this.add.text(0, 0, 'Totals\n' + PS.resourceText(totals), {
      font: '20px Indie Flower',
      fill: '#77BFA3',
      boundsAlignH: "center",
      boundsAlignV: "middle",
      wordWrap: true
    });
    totalText.setTextBounds(gameUiMap['3'].xLeft - 400, gameUiMap['3'].yTop - 300, gameUiMap['3'].xWidth, gameUiMap['3'].yHeight);

    branchText = this.add.text(0, 0, 'Branch\n*hover over node\nto see branch\nresources*', {
      font: '20px Indie Flower',
      fill: '#77BFA3',
      boundsAlignH: "center",
      boundsAlignV: "middle",
      wordWrap: true
    });
    branchText.setTextBounds(gameUiMap['4'].xLeft - 400, gameUiMap['4'].yTop - 300, gameUiMap['4'].xWidth, gameUiMap['4'].yHeight);

    // right - effects menu

    effectMenu = this.game.add.graphics(gameUiMap['2'].xLeft - 400, gameUiMap['2'].yTop - 300);
    effectMenu.beginFill(0x227744);
    effectMenu.drawRect(0, 0, gameUiMap['2'].xWidth, gameUiMap['2'].yHeight);
    effectMenu.endFill();

    effectMenuText = this.add.text(0, 0, '', {
      font: '20px Indie Flower',
      fill: '#77BFA3',
      boundsAlignH: "center",
      boundsAlignV: "middle"
    });
    effectMenuText.setTextBounds(gameUiMap['2'].xLeft - 400, gameUiMap['2'].yTop - 300, gameUiMap['2'].xWidth, gameUiMap['2'].yHeight);
    //effectMenuText.lineSpacing = -20;

    // right - submit button

    let submitBtn = this.add.text(0, 0, "submit", {
      font: '30px Indie Flower',
      fill: '#77BFA3',
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });
    submitBtn.setTextBounds(gameUiMap['5'].xLeft - 400, gameUiMap['5'].yTop - 300, gameUiMap['5'].xWidth, gameUiMap['5'].yHeight);

    submitBtn.inputEnabled = true;
    submitBtn.events.onInputOver.add(function() { submitBtn.fill = '#88CFB4'; submitBtn.setShadow(1.5,1.5,'rgb(0,0,0,0.15)', 3); }, this);
    submitBtn.events.onInputOut.add(function() { submitBtn.fill = '#77BFA3'; submitBtn.setShadow(null);}, this);
    submitBtn.events.onInputDown.add(this.submitSolution, this);


  }

  submitSolution() {
    PS.submitSolution({ getSocket: function() { return socket; }})(connections)();
  }

  drawNode(t) {
    return function(o) {
      return function() {
        const id = o.id;
        const x = o.x;
        const y = o.y;
        const nodeType = o.nodeType;
        const size = 15;

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

        const verify = PS.addConnectionJS({ from: click0
                                          , to: click1
                                          // TODO: this can be undefined
                                          , fromResources: resourceMap[click0.id]
                                          , connectedNodeIds: validNodes
                                          })(connections);
        console.log(verify);
        if (verify.added) {
          // draw line
          var line = this.game.add.graphics(0, 0, bgGroup);
          line.lineStyle(5, 0x000000);
          line.moveTo(click0.x, click0.y);
          line.lineTo(click1.x, click1.y);
          line.endFill();

          // add to connections
          connections = verify.newSolution;

          // calc totals
          var result = PS.calcResource(connections);
          console.log("totals:");
          console.log(result);
          this.setTotals(result);

          var vp = PS.calcVP(result)(connections);
          console.log("vp");
          console.log(vp);

          // add to furthest valid nodes
          // closest should already be a valid or start node
          validNodes.push(verify.furthestId);

          // update resources
          resourceMap[verify.furthestId] = verify.newResources;
        }
      }
    };
  }

  bulbOver(i, nodeType) {
    return function (bulb, pointer) {
      console.log("over " + i);
      effectMenuText.setText('ID: ' + i + '\n' + (PS.nodeTextJS(nodeType)));

      const res = resourceMap[i];
      if (typeof res !== "undefined") {
        const branchTextStr =
                'W:' + res.white +
                '\nB:' + res.blue + ' R:' + res.red +
                '\nG:' + res.green + 'Y:' + res.yellow;
        branchText.setText('Branch:\n' + branchTextStr);
      } else {
        branchText.setText('Branch\n*node\nnot\nconnected*');
      }
    };
  }

  bulbOut(i, nodeType) {
    return function (bulb, pointer) {
      console.log("out " + i);
      //mouseOverMenu.visible = false;
      //mouseOverText.text = '';
      //mouseOverText.visible = false;
      branchText.setText('Branch\n*hover\nover node\nto see\nbranch\nresources*');
    };
  }

  setTotals(x) {
    totals = x;
    totalText.setText('Totals\n' + PS.resourceText(totals));
  }

}
