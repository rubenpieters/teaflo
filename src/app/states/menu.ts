import { changeSelectedScreen, getSelectedScreen, addSelectedScreenCallback, addConnectedCallback } from "src/app/appstate";
import { addToSolution, addRestToSolution, removeCardFromSolution, removePathFromSolution, changeSolution, addSolutionCallback, addCardsCallback, LimitedCard, minusLimit, plusLimit } from "src/app/gamestate";
import { ServerConnection, connectToServer, getBoard } from "src/app/network/network";
import { Solution, SolutionResult, runSolution, runSolutionAll } from "src/shared/game/solution";
import { SolutionLog, showSolutionLog } from "src/shared/game/log";
import { showCrew } from "src/shared/game/crew";
import { IdCrew, IdEnemy, IdItem } from "src/shared/game/state";
import { GameState } from "src/shared/game/state";
import { config } from "src/app/config";
import { showItem } from "src/shared/game/item";
import { showEnemy } from "src/shared/game/enemy";


export type ValidResult = { state: GameState, log: SolutionLog };

let availableCardsCache: Phaser.Sprite[] = [];
let availableCardsTextCache: Phaser.Text[] = [];
let solutionCache: Phaser.Sprite[] = [];
let crewCache: Phaser.Sprite[] = [];
let itemCache: Phaser.Sprite[] = [];
let enemyCache: Phaser.Sprite[] = [];

let nodeTypeDetail: Phaser.Text;

// let serverConn: ServerConnection | undefined = undefined;

let playBoardGroup: Phaser.Group;

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

    this.game.canvas.oncontextmenu = e => e.preventDefault();
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
    // board1Btn.events.onInputDown.add(() => { if (serverConn !== undefined) { getBoard(serverConn, "1"); } });
    board1Btn.events.onInputDown.add(() => { getBoard(<any>undefined, "1"); });

    const board2Btn: Phaser.Text = this.add.text(0, 0, "board2", {
      font: "20px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "left",
      boundsAlignV: "middle",
    }, menuGroup);
    board2Btn.setTextBounds(175 - 400, 165 - 300, 75, 50);
    board2Btn.inputEnabled = true;
    // board2Btn.events.onInputDown.add(() => { if (serverConn !== undefined) { getBoard(serverConn, "2"); } });
    board2Btn.events.onInputDown.add(() => { getBoard(<any>undefined, "2"); });

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

    // right menu - background

    const rightMenu: Phaser.Graphics = this.game.add.graphics(550 - 400, 75 - 300, playGroup);
    rightMenu.beginFill(0x227744);
    rightMenu.drawRect(0, 0, 250, 285);
    rightMenu.endFill();

    // right menu

    nodeTypeDetail = this.game.add.text(0, 0, "--", {
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
    // undoBtn.events.onInputDown.add(undoAction);

    // redo button

    const redoBtn: Phaser.Text = this.add.text(0, 0, "R", {
      font: "22px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "left",
      boundsAlignV: "middle",
    }, playGroup);
    redoBtn.setTextBounds(775 - 400, 575 - 300, 25, 25);
    redoBtn.inputEnabled = true;
    // redoBtn.events.onInputDown.add(redoAction(this.game));

    // filter button

    const filterBtn: Phaser.Text = this.add.text(0, 0, "F", {
      font: "22px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "left",
      boundsAlignV: "middle",
    }, playGroup);
    filterBtn.setTextBounds(605 - 400, 575 - 300, 25, 25);
    filterBtn.inputEnabled = true;
    // filterBtn.events.onInputDown.add(filterAction);

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
    // stepRunMinBtn.events.onInputDown.add(stepRunAction(minus));

    // step run +1 button

    const stepRunBtn: Phaser.Text = this.add.text(0, 0, "+1>", {
      font: "22px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "left",
      boundsAlignV: "middle",
    }, playGroup);
    stepRunBtn.setTextBounds(675 - 400, 575 - 300, 40, 25);
    stepRunBtn.inputEnabled = true;
    // stepRunBtn.events.onInputDown.add(stepRunAction(x => x + 1));

    // start run button

    const startRunBtn: Phaser.Text = this.add.text(0, 0, ">>", {
      font: "22px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "left",
      boundsAlignV: "middle",
    }, playGroup);
    startRunBtn.setTextBounds(720 - 400, 575 - 300, 30, 25);
    startRunBtn.inputEnabled = true;
    // startRunBtn.events.onInputDown.add(startRunAction);

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

    addCardsCallback(cards => {
      mkAvailableCards(this.game, cards);
    });

    addSolutionCallback(solution => {
      const solutionResults = runSolutionAll(solution);
      mkSolution(this.game, resourcesText, solution, solutionResults);
      const solutionResult: SolutionResult | undefined = solutionResults[solutionResults.length - 1];
      if (solutionResult === undefined) {
        resourcesText.setText("/INVALID/");
        mkState(this.game, resourcesText, <ValidResult>runSolution({ paths: [] }));
      } else if (solutionResult.state === "invalid") {
        resourcesText.setText("/INVALID/");
        // TODO: use last valid solution result?
        mkState(this.game, resourcesText, <ValidResult>runSolution({ paths: [] }));
      } else {
        mkState(this.game, resourcesText, (<ValidResult>solutionResult));
      }
    });
    changeSolution({ paths: [] });

    // connect to server

    // serverConn = connectToServer((serverConn) => { getBoard(serverConn, "1"); });
    getBoard(<any>undefined, "1");
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


function mkAvailableCards(
  game: Phaser.Game,
  cards: LimitedCard[],
) {
  // clear old
  for (const sprite of availableCardsCache) {
    sprite.destroy();
  }
  for (const text of availableCardsTextCache) {
    text.destroy();
  }

  // create new
  const x = -250;
  let y = -250;
  const sprites: Phaser.Sprite[] = [];
  const texts: Phaser.Text[] = [];
  let i = 0;
  for (const card of cards) {
    const text = game.add.text(x + 50, y, card.limit.toString(), {
      font: "20px",
      fill: "#000000",
      boundsAlignH: "center",
      boundsAlignV: "middle"
    }, playBoardGroup);
    texts.push(text);

    const sprite = game.add.sprite(x, y, "card1", 0, playBoardGroup);
    sprite.inputEnabled = true;
    sprite.events.onInputDown.add(onAvailableCardClick(texts, card, i));
    sprite.events.onInputOver.add(() => {
      nodeTypeDetail.setText(JSON.stringify(card, undefined, 2));
    });
    sprites.push(sprite);

    y += 50;
    i += 1;
  }
  availableCardsCache = sprites;
  availableCardsTextCache = texts;
}

function onAvailableCardClick(
  texts: Phaser.Text[],
  card: LimitedCard,
  index: number,
) {
  return function() {
    const result = minusLimit(texts, index);
    if (result === "cardUsed") {
      switch (card.tag) {
        case "event": {
          addToSolution(card);
          break;
        }
        case "rest": {
          break;
        }
      }
    } else {
      console.log("card uses at 0");
    }
  };
}

function mkSolution(
  game: Phaser.Game,
  resourcesText: Phaser.Text,
  solution: Solution,
  solutionResults: SolutionResult[],
) {
  // clear old
  for (const sprite of solutionCache) {
    sprite.destroy();
  }

  // create new
  let x = 0;
  let y = 0;
  const sprites: Phaser.Sprite[] = [];
  let pathIndex = 0;
  let i = 0;
  for (const path of solution.paths) {
    // increase 1 for rest action
    i += 1;
    const sprite = game.add.sprite(x, y, "rest", 0, playBoardGroup);
    sprite.inputEnabled = true;
    sprites.push(sprite);
    sprite.events.onInputDown.add(removePathFromSolution(availableCardsTextCache, pathIndex));
    y -= 50;

    let cardIndex = 0;
    for (const card of path.eventCards) {
      const sprite = game.add.sprite(x, y, "card1", 0, playBoardGroup);
      sprite.inputEnabled = true;
      sprite.events.onInputOver.add(() => {
        nodeTypeDetail.setText(JSON.stringify(card, undefined, 2));
      });
      sprite.events.onInputDown.add(onSolutionCardClick(game, resourcesText, solutionResults, pathIndex, cardIndex, i, card.id));
      sprites.push(sprite);
      y -= 50;
      cardIndex += 1;
      i += 1;
    }

    if (pathIndex === solution.paths.length - 1) {
      const sprite = game.add.sprite(x, y, "slot", 0, playBoardGroup);
      sprites.push(sprite);
    }

    y = 0;
    x += 50;
    pathIndex += 1;
  }
  const sprite = game.add.sprite(x, y, "slot", 0, playBoardGroup);
  sprite.inputEnabled = true;
  sprite.events.onInputDown.add(() => addRestToSolution({ actions: [{ tag: "Rest" }], id: -1, tag: "rest" }));
  sprites.push(sprite);
  solutionCache = sprites;
}

function onSolutionCardClick(
  game: Phaser.Game,
  resourcesText: Phaser.Text,
  solutionResults: SolutionResult[],
  pathIndex: number,
  cardIndex: number,
  i: number,
  cardId: number,
) {
  return function(
    sprite: Phaser.Sprite,
    pointer: Phaser.Pointer,
  ) {
    if (pointer.leftButton.isDown) {
      _mkState(game, resourcesText, solutionResults, i);
    } else if (pointer.rightButton.isDown) {
      removeCardFromSolution(pathIndex, cardIndex);
      plusLimit(availableCardsTextCache, cardId);
    }
  };
}

function _mkState(
  game: Phaser.Game,
  resourcesText: Phaser.Text,
  solutionResults: SolutionResult[],
  index: number,
) {
  let solutionResult: SolutionResult;
  if (index >= solutionResults.length) {
    solutionResult = solutionResults[solutionResults.length - 1];
  } else {
    solutionResult = solutionResults[index];
  }
  if (solutionResult.state === "invalid") {
    resourcesText.setText("/INVALID/");
  } else {
    mkState(game, resourcesText, (<ValidResult>solutionResult));
  }
}

function mkState(
  game: Phaser.Game,
  resourcesText: Phaser.Text,
  solutionResult: ValidResult,
) {
  resourcesText.setText("gold: " + solutionResult.state.gold);
  const crew: IdCrew[] = solutionResult.state.crew;
  const items: IdItem[] = solutionResult.state.items;
  const enemies: IdEnemy[] = solutionResult.state.enemies;

  // clear old
  for (const sprite of crewCache) {
    sprite.destroy();
  }
  for (const sprite of itemCache) {
    sprite.destroy();
  }
  for (const sprite of enemyCache) {
    sprite.destroy();
  }


  // create new
  let x = 0;
  let y = 100;
  let sprites: Phaser.Sprite[] = [];
  for (const ally of crew) {
    const sprite = game.add.sprite(x, y, "ally", 0, playBoardGroup);
    sprite.inputEnabled = true;
    sprite.events.onInputOver.add(() => {
      nodeTypeDetail.setText(JSON.stringify(showCrew(ally), undefined, 2));
    });
    sprites.push(sprite);
    x += 50;
  }
  crewCache = sprites;

  y += 50;
  x = 0;
  sprites = [];
  for (const item of items) {
    const sprite = game.add.sprite(x, y, "item", 0, playBoardGroup);
    sprite.inputEnabled = true;
    sprite.events.onInputOver.add(() => {
      nodeTypeDetail.setText(JSON.stringify(showItem(item), undefined, 2));
    });
    sprites.push(sprite);
    x += 50;
  }
  itemCache = sprites;

  y += 50;
  x = 0;
  sprites = [];
  for (const enemy of enemies) {
    const sprite = game.add.sprite(x, y, "ally", 0, playBoardGroup);
    sprite.inputEnabled = true;
    sprite.events.onInputOver.add(() => {
      nodeTypeDetail.setText(JSON.stringify(showEnemy(enemy), undefined, 2));
    });
    sprites.push(sprite);
    x += 50;
  }
  enemyCache = sprites;
}