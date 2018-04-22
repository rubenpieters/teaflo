import { changeSelectedScreen, getSelectedScreen, addSelectedScreenCallback, addConnectedCallback, addBoardCallback } from "src/app/appstate";
import { connectToServer } from "src/app/network/network";
import { Node } from "src/shared/node";
import { Board } from "src/shared/board";
import { ConnectResult, Solution } from "src/shared/connectResult";
import { verifyAndAddConnection } from "src/shared/solution";

let playBoardGroup: Phaser.Group;


let validFromNodes: number[] = [];
let solution: Solution = {};


type ClickStateFrom = {
  tag: "ClickStateFrom",
};

type ClickStateTo = {
  tag: "ClickStateTo",
  fromNode: Node,
};

type ClickState = ClickStateFrom | ClickStateTo;

let clickState: ClickState = { tag: "ClickStateFrom" };




let zoom: number = 0;

const gameX: number = 2500;
const gameY: number = 2000;

let downLocX: number | undefined;
let downLocY: number | undefined;
let oldCamX: number | undefined;
let oldCamY: number | undefined;

let viewRect: Phaser.Rectangle;

export default class Menu extends Phaser.State {
  public create(): void {
    this.stage.backgroundColor = 0xDCDCDC;

    const top1: Phaser.Text = this.add.text(0, 0, "Home", {
      font: "60px Indie Flower",
      fill: "#F08080",
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });
    top1.setTextBounds(0 - 400, 0 - 300, 250, 100);
    top1.inputEnabled = true;
    top1.events.onInputDown.add(() => changeSelectedScreen("Home"));

    const top2: Phaser.Text = this.add.text(0, 0, "Play", {
      font: "60px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });
    top2.setTextBounds(250 - 400, 0 - 300, 250, 100);
    top2.inputEnabled = true;
    top2.events.onInputDown.add(() => changeSelectedScreen("Play"));

    const connectionIndicator: Phaser.Text = this.add.text(620 - 400, 10 - 300, "No Connection", {
      font: "15px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });

    // Menu Screen

    const menuGroup = this.game.add.group();

    const home1: Phaser.Text = this.add.text(0, 0, "Current Board", {
      font: "35px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "left",
      boundsAlignV: "middle",
    }, menuGroup);
    home1.setTextBounds(50 - 400, 100 - 300, 150, 75);

    const home2: Phaser.Text = this.add.text(0, 0, "Top Solutions", {
      font: "35px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "left",
      boundsAlignV: "middle",
    }, menuGroup);
    home2.setTextBounds(50 - 400, 200 - 300, 150, 75);

    // Play Screen

    const playGroup = this.game.add.group();

    const playResourceMenu = this.game.add.graphics(0, 600, playGroup);
    playResourceMenu.beginFill(0x227744);
    playResourceMenu.drawRect(0, 0, 800, 200);
    playResourceMenu.endFill();

    playGroup.visible = false;

    playBoardGroup = this.game.add.group(playGroup);
    this.game.world.setBounds(-1000, -1000, 2000, 2000);
    viewRect = new Phaser.Rectangle(0, 100, this.game.width, this.game.height);

    playBoardGroup.position.setTo(this.game.world.centerX, this.game.world.centerY);

    this.game.camera.x = this.game.width * -0.5;
    this.game.camera.y = this.game.height * -0.5;

    this.game.input.onDown.add(onDown(this.game), this);

    // callbacks

    // general - callbacks

    addSelectedScreenCallback(screen => { switch (screen) {
      case "Home": {
        top1.fill = "#F08080";
        top2.fill = "#77BFA3";
        menuGroup.visible = true;
        playGroup.visible = false;
        break;
      }
      case "Play": {
        top1.fill = "#77BFA3";
        top2.fill = "#F08080";
        menuGroup.visible = false;
        playGroup.visible = true;
        break;
      }
    }});

    addConnectedCallback(connected => {
      if (connected) {
        connectionIndicator.setText("Connected");
      } else {
        connectionIndicator.setText("No Connection");
      }
    });

    // play - callbacks

    addBoardCallback(board => {
      drawBoard(this.game, playBoardGroup, board);
      validFromNodes = [board[0].id];
    });

    connectToServer();
  }

  public update() {
    switch (getSelectedScreen()) {
      case "Play": {
        gameUpdate(this.game);
        break;
      }
      case "Home": {
        break;
      }
    }
  }
}

function gameUpdate(game: Phaser.Game) {
  if (game.input.activePointer.isDown) {
    const currentX: number = game.input.activePointer.x;
    const currentY: number = game.input.activePointer.y;

    if (downLocX !== undefined && downLocY !== undefined && oldCamX !== undefined && oldCamY !== undefined) {
      const diffX = currentX - downLocX;
      const diffY = currentY - downLocY;

      playBoardGroup.pivot.x = Phaser.Math.clamp(oldCamX - diffX * (0.5 / zoom), -gameX, gameX);
      playBoardGroup.pivot.y = Phaser.Math.clamp(oldCamY - diffY * (0.5 / zoom), -gameY, gameY);
    }
  }

  if (game.input.keyboard.isDown(Phaser.Keyboard.Q)) {
    zoom += 0.05;
  }
  if (game.input.keyboard.isDown(Phaser.Keyboard.A)) {
    zoom -= 0.05;
  }
  zoom = Phaser.Math.clamp(zoom, 0.3, 2);

  playBoardGroup.scale.set(zoom);

  playBoardGroup.forEachExists((node: Phaser.Sprite) => {
    const boundsPoint = new Phaser.Point(
      ((node.x - playBoardGroup.pivot.x) * playBoardGroup.scale.x) + (game.width * 0.5),
      ((node.y - playBoardGroup.pivot.y) * playBoardGroup.scale.y) + (game.height * 0.5)
    );

    if (Phaser.Rectangle.containsPoint(viewRect, boundsPoint)) {
      node.visible = true;
    } else {
      node.visible = false;
    }
  });
}

function onDown(game: Phaser.Game) {
  return function() {
    switch (getSelectedScreen()) {
      case "Play": {
        downLocX = game.input.activePointer.x;
        downLocY = game.input.activePointer.y;
        oldCamX = playBoardGroup.pivot.x;
        oldCamY = playBoardGroup.pivot.y;
        break;
      }
      case "Home": {
        break;
      }
    }
  };
}

function nodeClick(game: Phaser.Game, node: Node) {
  return function(nodeSprite: Phaser.Sprite) {
    switch (clickState.tag) {
      case "ClickStateFrom": {
        console.log("click0: " + node.id);
        if (validFromNodes.filter(x => x === node.id).length < 1) {
          console.log("not valid from node");
        } else {
          clickState = { tag: "ClickStateTo", fromNode: node };
        }
        break;
      }
      case "ClickStateTo": {
        console.log("click1: " + node.id);
        const fromNode: Node = clickState.fromNode;
        const toNode: Node = node;

        const connectResult: ConnectResult = verifyAndAddConnection(fromNode, toNode, validFromNodes, solution);
        switch (connectResult.tag)  {
          case "InvalidFromNode": {
            console.log("invalid from node");
            break;
          }
          case "InvalidAngle": {
            console.log("invalid angle");
            break;
          }
          case "ValidConnection": {
            makeConnection(game, fromNode, toNode);
            solution = connectResult.newSolution;
            validFromNodes = connectResult.newValidFromNodes;
            break;
          }
        }

        clickState = { tag: "ClickStateFrom" };
        break;
      }
    }
  };
}

function makeConnection(game: Phaser.Game, fromNode: Node, toNode: Node) {
  const line = game.add.graphics(0, 0, playBoardGroup);
  line.lineStyle(5, 0x000000);
  line.moveTo(fromNode.x, fromNode.y);
  line.lineTo(toNode.x, toNode.y);
  line.endFill();
}

function drawBoard(game: Phaser.Game, group: Phaser.Group, board: Board): void {
  console.log("drawing board");
  for (const node of board) {
    drawNode(game, group, node);
  }
}

function drawNode(game: Phaser.Game, group: Phaser.Group, node: Node): void {
  const size: number = 15;

  const nodeSprite: Phaser.Graphics = game.add.graphics(node.x, node.y, group);
  nodeSprite.beginFill(0xFF0000);
  nodeSprite.drawRect(size * -0.5, size * -0.5, size, size);
  nodeSprite.endFill();
  nodeSprite.inputEnabled = true;
  nodeSprite.events.onInputOver.add(() => { console.log("id: " + node.id); });
  nodeSprite.events.onInputDown.add(nodeClick(game, node));
}