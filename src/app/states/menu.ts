import { changeSelectedScreen, getSelectedScreen, addSelectedScreenCallback, addConnectedCallback, addBoardCallback, nodeLocation, changeCurrentFilter, addCurrentFilterCallback } from "src/app/appstate";
import { changeSelectedNode, addNodeCallback, changeShownResources, addShownResourcesCallback } from "src/app/gamestate";
import { ServerConnection, connectToServer, getBoard } from "src/app/network/network";
import { Node } from "src/shared/node";
import { Board, filterBoard } from "src/shared/board";
import { ConnectResult, Solution } from "src/shared/connectResult";
import { verifyAndAddConnection, initVisit } from "src/shared/solution";
import { History, Action } from "src/app/history/history";
import { showNodeType } from "src/shared/nodeType";
import { showModifier } from "src/shared/rules/modifier";

import { config } from "src/app/config";

let serverConn: ServerConnection | undefined = undefined;

let playBoardGroup: Phaser.Group;

type BoardNode = {
  sprite: Phaser.Graphics,
};
let nodes: { [nodeId: number]: BoardNode } = {};

let currentLimit: number = 0;
let circle: Phaser.Text | undefined = undefined;

const undoList: History = [];
let redoList: History = [];


let validFromNodes: number[] = [];
let solution: Solution = {};
const connectionSprites: Phaser.Graphics[] = [];
let nodeSprites: Phaser.Graphics[] = [];


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
  public init(): void {
    this.scale.scaleMode = Phaser.ScaleManager.SHOW_ALL;
    this.scale.maxWidth = config.gameWidth;
    this.scale.maxHeight = config.gameHeight;
    this.scale.pageAlignHorizontally = true;
    this.scale.pageAlignVertically = true;
  }

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

    const board1Btn: Phaser.Text = this.add.text(0, 0, "board1", {
      font: "20px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "left",
      boundsAlignV: "middle",
    }, menuGroup);
    board1Btn.setTextBounds(75 - 400, 165 - 300, 75, 50);
    board1Btn.inputEnabled = true;
    board1Btn.events.onInputDown.add(() => { if (serverConn !== undefined) { getBoard(serverConn, "ABCD-EFGH"); } });

    const board2Btn: Phaser.Text = this.add.text(0, 0, "board2", {
      font: "20px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "left",
      boundsAlignV: "middle",
    }, menuGroup);
    board2Btn.setTextBounds(175 - 400, 165 - 300, 75, 50);
    board2Btn.inputEnabled = true;
    board2Btn.events.onInputDown.add(() => { if (serverConn !== undefined) { getBoard(serverConn, "IJKL-MNOP"); } });

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

    // circle

    circle = this.add.text(0, 0, "O", {
      font: "40px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "left",
      boundsAlignV: "middle",
    }, playBoardGroup);
    circle.setTextBounds(-13, -13, 26, 26);

    // right menu - background

    const rightMenu: Phaser.Graphics = this.game.add.graphics(550 - 400, 75 - 300, playGroup);
    rightMenu.beginFill(0x227744);
    rightMenu.drawRect(0, 0, 250, 285);
    rightMenu.endFill();

    // right menu

    const nodeTypeDetail: Phaser.Text = this.game.add.text(0, 0, "--", {
      font: "14px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "center",
      boundsAlignV: "middle"
    }, playGroup);
    nodeTypeDetail.setTextBounds(570 - 400, 90 - 300, 210, 245);

    // bottom menu - background

    const bottomMenu: Phaser.Graphics = this.game.add.graphics(0 - 400, 450 - 300, playGroup);
    bottomMenu.beginFill(0x227744);
    bottomMenu.drawRect(0, 0, 800, 150);
    bottomMenu.endFill();

    // bottom menu

    /*const nodeTypeTitle: Phaser.Text = this.game.add.text(0, 0, "Node Type", {
      font: "20px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "center",
      boundsAlignV: "middle"
    }, playGroup);
    nodeTypeTitle.setTextBounds(10 - 400, 425 - 300, 100, 25);

    const nodeTypeText: Phaser.Text = this.game.add.text(0, 0, "--", {
      font: "20px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "center",
      boundsAlignV: "middle"
    }, playGroup);
    nodeTypeText.setTextBounds(10 - 400, 450 - 300, 100, 25);*/

    const resourcesTitle: Phaser.Text = this.game.add.text(0, 0, "Resources", {
      font: "20px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "center",
      boundsAlignV: "middle"
    }, playGroup);
    resourcesTitle.setTextBounds(100 - 400, 425 - 300, 100, 25);

    const resourcesText: Phaser.Text = this.game.add.text(0, 0, "--", {
      font: "14px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "center",
      boundsAlignV: "middle",
    }, playGroup);
    resourcesText.setTextBounds(100 - 400, 450 - 300, 100, 150);

    const modsTitle: Phaser.Text = this.game.add.text(0, 0, "Mods", {
      font: "20px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "center",
      boundsAlignV: "middle"
    }, playGroup);
    modsTitle.setTextBounds(260 - 400, 425 - 300, 100, 25);

    const modsText: Phaser.Text = this.game.add.text(0, 0, "--", {
      font: "14px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "center",
      boundsAlignV: "middle",
    }, playGroup);
    modsText.setTextBounds(260 - 400, 450 - 300, 100, 150);

    // undo button

    const undoBtn: Phaser.Text = this.add.text(0, 0, "U", {
      font: "22px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "left",
      boundsAlignV: "middle",
    }, playGroup);
    undoBtn.setTextBounds(750 - 400, 575 - 300, 25, 25);
    undoBtn.inputEnabled = true;
    undoBtn.events.onInputDown.add(undoAction);

    // redo button

    const redoBtn: Phaser.Text = this.add.text(0, 0, "R", {
      font: "22px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "left",
      boundsAlignV: "middle",
    }, playGroup);
    redoBtn.setTextBounds(775 - 400, 575 - 300, 25, 25);
    redoBtn.inputEnabled = true;
    redoBtn.events.onInputDown.add(redoAction(this.game));

    // filter button

    const filterBtn: Phaser.Text = this.add.text(0, 0, "F", {
      font: "22px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "left",
      boundsAlignV: "middle",
    }, playGroup);
    filterBtn.setTextBounds(605 - 400, 575 - 300, 25, 25);
    filterBtn.inputEnabled = true;
    filterBtn.events.onInputDown.add(filterAction);

    // step run -1 button

    const stepRunMinBtn: Phaser.Text = this.add.text(0, 0, "-1>", {
      font: "22px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "left",
      boundsAlignV: "middle",
    }, playGroup);
    stepRunMinBtn.setTextBounds(630 - 400, 575 - 300, 40, 25);
    stepRunMinBtn.inputEnabled = true;
    const minus = (x: number) => { if (x - 1 > 0) {  return x - 1; } else { return x; } };
    stepRunMinBtn.events.onInputDown.add(stepRunAction(minus));

    // step run +1 button

    const stepRunBtn: Phaser.Text = this.add.text(0, 0, "+1>", {
      font: "22px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "left",
      boundsAlignV: "middle",
    }, playGroup);
    stepRunBtn.setTextBounds(675 - 400, 575 - 300, 40, 25);
    stepRunBtn.inputEnabled = true;
    stepRunBtn.events.onInputDown.add(stepRunAction(x => x + 1));

    // start run button

    const startRunBtn: Phaser.Text = this.add.text(0, 0, ">>", {
      font: "22px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "left",
      boundsAlignV: "middle",
    }, playGroup);
    startRunBtn.setTextBounds(720 - 400, 575 - 300, 30, 25);
    startRunBtn.inputEnabled = true;
    startRunBtn.events.onInputDown.add(startRunAction);

    // callbacks

    // callbacks - general

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
        if (circle !== undefined) {
          circle.visible = false;
        }
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

    // callbacks - play

    addBoardCallback(board => {
      nodeSprites = drawBoard(this.game, playBoardGroup, board);
      validFromNodes = [board[0].id];
    });

    addShownResourcesCallback(result => {
      const stepData = result.values;
      modsText.setText(stepData.modifiers.map(showModifier).join("\n"));
      let resourcesString: string = "";
      if (! result.valid) {
        resourcesString = "INVALID\n";
      } else {
        resourcesString = resourcesString +
          "Basic Total: " + stepData.resources.Basic.Total + "\n" +
          "Basic Temp: " + stepData.resources.Basic.Temp + "\n" +
          "Stack Total: " + stepData.resources.Stack.Total + "\n" +
          "Stack Temp: " + stepData.resources.Stack.Temp + "\n" +
          "Victory Temp: " + stepData.resources.Victory.Temp + "\n" +
          "Victory Total: " + stepData.resources.Victory.Total + "\n" +
          "Growth: " + stepData.growth;
      }
      resourcesText.setText(resourcesString);
    });

    addNodeCallback(node => {
      // nodeTypeText.setText(nodeType.meta.name);
      nodeTypeDetail.setText(showNodeType(node));
    });

    addCurrentFilterCallback((input: {board: Node[], filter: string | undefined }) => {
      if (input.filter === undefined) {
        console.log("CLEAR");
        playBoardGroup.forEachExists((node: Phaser.Sprite) => {
          node.data.visibleOverride = false;
        });
      } else {
        const nodeIds: number[] = filterBoard(input.filter, input.board).map(x => x.id);
        console.log(nodeIds);
        playBoardGroup.forEachExists((node: Phaser.Sprite) => {
          node.data.visibleOverride = true;
        });
        for (const nodeId in nodeIds) {
          nodes[nodeId].sprite.data.visibleOverride = false;
        }
      }
    });

    serverConn = connectToServer((serverConn) => { getBoard(serverConn, "ABCD-EFGH"); });
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

    if (Phaser.Rectangle.containsPoint(viewRect, boundsPoint) && ! node.data.visibleOverride) {
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

        const connectResult: ConnectResult = verifyAndAddConnection(fromNode, toNode, connectionSprites.length, validFromNodes, solution);
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
            undoList.push({ tag: "AddConnectionAction", from: fromNode, to: toNode });
            redoList = [];
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
  const line: Phaser.Graphics = game.add.graphics(0, 0, playBoardGroup);
  line.lineStyle(5, 0x000000);
  line.moveTo(fromNode.x, fromNode.y);
  line.lineTo(toNode.x, toNode.y);
  line.endFill();
  connectionSprites.push(line);
}

function drawBoard(game: Phaser.Game, group: Phaser.Group, board: Board): Phaser.Graphics[] {
  const result: Phaser.Graphics[] = [];
  nodes = {};
  for (const node of board) {
    const sprite = drawNode(game, group, node);
    result.push(sprite);
    nodes[node.id] = { sprite: sprite };
  }
  return result;
}

function drawNode(game: Phaser.Game, group: Phaser.Group, node: Node): Phaser.Graphics {
  const size: number = 15;

  const nodeSprite: Phaser.Graphics = game.add.graphics(node.x, node.y, group);
  nodeSprite.beginFill(node.nodeType.meta.color);
  nodeSprite.drawRect(size * -0.5, size * -0.5, size, size);
  nodeSprite.endFill();
  nodeSprite.inputEnabled = true;
  nodeSprite.events.onInputOver.add(() => { changeSelectedNode(node.nodeType); });
  nodeSprite.events.onInputDown.add(nodeClick(game, node));
  return nodeSprite;
}

function undoAction() {
  const [lastAction] = undoList.splice(-1, 1);
  if (lastAction === undefined) {
    // no undo action, do nothing
  } else {
    switch (lastAction.tag) {
      case "AddConnectionAction": {
        const index: number = validFromNodes.indexOf(lastAction.to.id);
        if (index === -1) {
          throw "Should not happen: to id " + lastAction.to.id + " was not added to valid from nodes";
        } else {
          validFromNodes.splice(index, 1);
        }
        solution[lastAction.from.id].splice(-1, 1);
        const [connectionSprite] = connectionSprites.splice(-1, 1);
        connectionSprite.destroy();
        break;
      }
    }

    // add action to redo list
    redoList.push(lastAction);
  }
}

function redoAction(game: Phaser.Game) {
  return function() {
    const [lastAction] = redoList.splice(-1, 1);
    if (lastAction === undefined) {
      // no redo action, do nothing
    } else {
      switch (lastAction.tag) {
        case "AddConnectionAction": {
          const connectResult: ConnectResult = verifyAndAddConnection(lastAction.from, lastAction.to, connectionSprites.length, validFromNodes, solution);
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
              makeConnection(game, lastAction.from, lastAction.to);
              solution = connectResult.newSolution;
              validFromNodes = connectResult.newValidFromNodes;
              break;
            }
          }
          break;
        }
      }

      undoList.push(lastAction);
    }
  };
}

function startRunAction() {
  const stepResult = initVisit(solution, Number.POSITIVE_INFINITY);
  const xy = nodeLocation(stepResult.lastVisitedNodeId);
  if (circle !== undefined) {
    circle.position.set(xy.x, xy.y);
    circle.visible = true;
  }
  changeShownResources({ values: stepResult.stepValues, valid: stepResult.validSolution });
}

function stepRunAction(f: (n: number) => number) {
  return function() {
    currentLimit = f (currentLimit);
    console.log("LIMIT: " + currentLimit);
    const stepResult = initVisit(solution, currentLimit);
    const xy = nodeLocation(stepResult.lastVisitedNodeId);
    if (circle !== undefined) {
      circle.position.set(xy.x, xy.y);
      circle.visible = true;
    }
    changeShownResources({ values: stepResult.stepValues, valid: stepResult.validSolution });
  };
}

function filterAction() {
  const input = prompt("change filter to ('clear' to clear):");
  if (input === null || input === "") {
    // do nothing
  } else if (input === "clear") {
    changeCurrentFilter(undefined);
  } else {
    changeCurrentFilter(input);
  }
}