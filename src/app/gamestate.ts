import { focus, over, set } from "src/shared/iassign-util";
import { Solution, runSolutionAll } from "src/shared/game/solution";
import { Card, Rest, Event } from "src/shared/game/card";
import { GameState } from "src/shared/game/state";
import { abilityToAction } from "src/shared/game/ability";

export type Limit = {
  limit: number,
};

export type LimitedCard = Card & Limit;

export type LeftMenuOption = "crew" | "enemy" | "item" | "general" | "rest";

export const allLeftMenuOptions: LeftMenuOption[] = ["crew", "enemy", "item", "general", "rest"];

export type Board = {
  availableCards: LimitedCard[],
  solution: Solution,
  selectedLeftMenu: LeftMenuOption,
  graphics: BoardGraphics,
  game: Phaser.Game,
  group: Phaser.Group,
};

type BoardGraphics = {
  leftMenuTabs: Phaser.Graphics[],
  availableCardsGfx: AvailableCardGfx[],
  solutionGfx: Phaser.Graphics[],
  stateGfx: Phaser.Graphics[],
};

export function newBoard(
  game: Phaser.Game,
  group: Phaser.Group,
) {
  const board: Board = {
    solution: { paths: [] },
    availableCards: [],
    selectedLeftMenu: "crew",
    graphics: {
      leftMenuTabs: [],
      availableCardsGfx: [],
      solutionGfx: [],
      stateGfx: [],
    },
    game,
    group,
  };

  // left menu

  const leftMenu: Phaser.Graphics = game.add.graphics(0, 75, group);
  leftMenu.beginFill(0x227744);
  leftMenu.drawRect(0, 0, 200, 525);
  leftMenu.endFill();

  // left menu tabs

  board.graphics.leftMenuTabs = allLeftMenuOptions.map((ct, i) => mkLeftMenuTab(board, ct, i));

  board.graphics.availableCardsGfx = popLeftMenu(board);

  // right menu

  const rightMenu: Phaser.Graphics = game.add.graphics(600, 40, group);
  rightMenu.beginFill(0x227744);
  rightMenu.drawRect(0, 0, 200, 560);
  rightMenu.endFill();

  chLeftMenuTab(board, "crew");
  return board;
}

function mkLeftMenuTab(
  board: Board,
  cardType: LeftMenuOption,
  i: number
) {
  const leftMenuTab: Phaser.Graphics = board.game.add.graphics((40 * i), 35, board.group);
  leftMenuTab.beginFill(0x227744);
  leftMenuTab.drawRect(0, 0, 40, 40);
  leftMenuTab.endFill();
  leftMenuTab.tint = 0xFFFFFF;
  leftMenuTab.inputEnabled = true;
  leftMenuTab.events.onInputDown.add(() => chLeftMenuTab(board, cardType));
  return leftMenuTab;
}

function chLeftMenuTab(
  board: Board,
  cardType: LeftMenuOption,
) {
  board.selectedLeftMenu = cardType;
  board.graphics.leftMenuTabs.map(g => g.tint = 0xFFFFFF);
  board.graphics.leftMenuTabs[allLeftMenuOptions.indexOf(cardType)].tint = 0xAAAAAA;
  board.graphics.availableCardsGfx = popLeftMenu(board);
}

type AvailableCardGfx = {
  title: Phaser.Graphics,
  limitText: Phaser.Text,
  effects: Phaser.Graphics[],
};

export function chAvailableCards(
  board: Board,
  availableCards: LimitedCard[],
) {
  board.availableCards = availableCards;
  board.graphics.availableCardsGfx = popLeftMenu(board);
}

function popLeftMenu(
  board: Board,
): AvailableCardGfx[] {
  // clear old
  console.log("s: " + board.graphics.availableCardsGfx.length);
  for (const gfx of board.graphics.availableCardsGfx) {
    gfx.title.destroy();
    gfx.limitText.destroy();
    gfx.effects.map(x => x.destroy());
  }

  const filteredCards = board.availableCards
    .filter((card) => card.subtag === board.selectedLeftMenu);

  const x = 5;
  let y = 80;
  const gfx: AvailableCardGfx[] = [];
  for (const card of filteredCards) {
    const limitText = board.game.add.text(x + 170, y, card.limit.toString(), {
      font: "20px",
      fill: "#000000",
      boundsAlignH: "center",
      boundsAlignV: "middle"
    }, board.group);

    const title: Phaser.Graphics = board.game.add.graphics(x, y, board.group);
    title.beginFill(0x449966);
    title.drawRect(0, 0, 165, 30);
    title.endFill();
    title.inputEnabled = true;
    title.events.onInputDown.add(() => addToSolution(board, card));
    // title.events.onInputDown.add(onAvailableCardClick(text, card, card.index));
    y += 35;

    const effects: Phaser.Graphics[] = [];
    let i = 0;
    for (const action of card.actions) {
      const effect: Phaser.Graphics = board.game.add.graphics(x, y, board.group);
      effect.beginFill(0x66BB88);
      effect.drawRect(0, 0, 165, 20);
      effect.endFill();
      effect.tint = 0xFFFFFF;
      effect.inputEnabled = true;
      effect.events.onInputDown.add(() => console.log("id: " + card.id + " index " + i));
      effects.push(effect);

      y += 25;
      i += 1;
    }

    gfx.push({ title, limitText, effects });
  }
  return gfx;
}

function addToSolution(
  board: Board,
  card: Card,
) {
  if (card.id !== "created") {
    const index = board.availableCards.findIndex(c => c.id === card.id);
    if (board.availableCards[index].limit <= 0) {
      return "noUses";
    }
    board.availableCards[index].limit -= 1;
  }
  switch (card.tag) {
    case "event": {
      if (board.solution.paths.length === 0) {
        return "noPaths";
      } else {
        board.solution.paths[board.solution.paths.length - 1].eventCards.push(card);
      }
      break;
    }
    case "rest": {
      board.solution.paths.push({ restCard: card, eventCards: [] });
      break;
    }
  }

  mkSolution(board);
  chLeftMenuTab(board, board.selectedLeftMenu);
  return "cardAdded";
}

function removeEventFromSolution(
  board: Board,
  card: Card,
  pathIndex: number,
  eventIndex: number,
) {
  board.solution.paths[pathIndex].eventCards =
    board.solution.paths[pathIndex].eventCards.slice(0, eventIndex).concat(
      board.solution.paths[pathIndex].eventCards.slice(eventIndex + 1, board.solution.paths[pathIndex].eventCards.length)
    );
  if (card.id !== "created") {
    const index = board.availableCards.findIndex(c => c.id === card.id);
    board.availableCards[index].limit += 1;
  }
  chLeftMenuTab(board, board.selectedLeftMenu);
  mkSolution(board);
}

function removeRestFromSolution(
  board: Board,
  card: Card,
  pathIndex: number,
) {
  for (const card of board.solution.paths[pathIndex].eventCards) {
    if (card.id !== "created") {
      const index = board.availableCards.findIndex(c => c.id === card.id);
      board.availableCards[index].limit += 1;
    }
  }
  board.solution.paths =
    board.solution.paths.slice(0, pathIndex).concat(
      board.solution.paths.slice(pathIndex + 1, board.solution.paths.length)
    );
  const index = board.availableCards.findIndex(c => c.id === card.id);
  board.availableCards[index].limit += 1;
  chLeftMenuTab(board, board.selectedLeftMenu);
  mkSolution(board);
}

function mkSolution(
  board: Board,
  // solutionResults: SolutionResult[],
) {
  // clear old
  for (const sprite of board.graphics.solutionGfx) {
    sprite.destroy();
  }

  // create new
  let x = 230;
  let y = 350;
  const sprites: Phaser.Graphics[] = [];
  let pathIndex = 0;
  let i = 0;
  for (const path of board.solution.paths) {
    // increase 1 for rest action
    i += 1;
    const sprite: Phaser.Graphics = board.game.add.graphics(x, y, board.group);
    sprite.beginFill(0x223377);
    sprite.drawRect(0, 0, 40, 20);
    sprite.endFill();
    sprite.inputEnabled = true;
    sprite.events.onInputDown.add(onSolutionRestCardClick(board, path.restCard, pathIndex));
    sprites.push(sprite);
    y -= 25;

    let cardIndex = 0;
    for (const card of path.eventCards) {
      const sprite: Phaser.Graphics = board.game.add.graphics(x, y, board.group);
      sprite.beginFill(0x223377);
      sprite.drawRect(0, 0, 40, 20);
      sprite.endFill();
      sprite.inputEnabled = true;
      /*sprite.events.onInputOver.add(() => {
        nodeTypeDetail.setText(JSON.stringify(card, undefined, 2));
      });*/
      sprite.events.onInputDown.add(onSolutionEventCardClick(board, card, pathIndex, cardIndex));
      sprites.push(sprite);

      y -= 25;
      cardIndex += 1;
      i += 1;
    }

    /*if (pathIndex === board.solution.paths.length - 1) {
      const sprite = board.game.add.sprite(x, y, "slot", 0, board.group);
      sprites.push(sprite);
    }*/

    y = 350;
    x += 45;
    pathIndex += 1;
  }
  /* const sprite = board.game.add.sprite(x, y, "slot", 0, board.group);
  sprite.inputEnabled = true; */
  // sprite.events.onInputDown.add(() => addRestToSolution({ actions: [{ tag: "Rest" }], id: -1, tag: "rest", subtag: "rest" }));
  // sprites.push(sprite);
  board.graphics.solutionGfx = sprites;

  // update solution

  const solutionResults = runSolutionAll(board.solution);
  if (solutionResults.length === 0) {
    console.log("empty state");
    clearState(board);
  } else {
    const lastResult = solutionResults[solutionResults.length - 1];
    console.log(lastResult.log);
    if (lastResult.state === "invalid") {
      console.log("invalid state");
      clearState(board);
    } else {
      mkState(board, lastResult.state);
    }
  }
}

function onSolutionRestCardClick(
  board: Board,
  card: Rest,
  pathIndex: number,
) {
  return function(
    sprite: Phaser.Sprite,
    pointer: Phaser.Pointer,
  ) {
    if (pointer.leftButton.isDown) {
      // show solution up to here
    } else if (pointer.rightButton.isDown) {
      removeRestFromSolution(board, card, pathIndex);
    }
  };
}

function onSolutionEventCardClick(
  board: Board,
  card: Event,
  pathIndex: number,
  eventIndex: number,
) {
  return function(
    sprite: Phaser.Sprite,
    pointer: Phaser.Pointer,
  ) {
    if (pointer.leftButton.isDown) {
      // show solution up to here
    } else if (pointer.rightButton.isDown) {
      removeEventFromSolution(board, card, pathIndex, eventIndex);
    }
  };
}

function clearState(
  board: Board,
) {
  for (const sprite of board.graphics.stateGfx) {
    sprite.destroy();
  }
}

function mkState(
  board: Board,
  gs: GameState
) {
  clearState(board);

  let x = 330;
  const y = 420;
  let allyId = 0;
  const sprites: Phaser.Graphics[] = [];
  for (const ally of gs.crew) {
    const sprite: Phaser.Graphics = board.game.add.graphics(x, y, board.group);
    sprite.beginFill(0x4477CC);
    sprite.drawRect(0, 0, 20, 40);
    sprite.endFill();
    sprites.push(sprite);

    const hpRatio = ally.hp / ally.maxHp;
    const hpSprite: Phaser.Graphics = board.game.add.graphics(x, y + 45, board.group);
    hpSprite.beginFill(0xFF0000);
    hpSprite.drawRect(0, 0, 20 * hpRatio, 5);
    hpSprite.endFill();
    sprites.push(hpSprite);

    for (const ability of ally.abilities) {
      const sprite: Phaser.Graphics = board.game.add.graphics(x, y + 75, board.group);
      sprite.beginFill(0x449966);
      sprite.drawRect(0, 0, 20, 15);
      sprite.endFill();
      sprite.inputEnabled = true;
      const abilityCard: Card = {
        id: "created",
        actions: [
          abilityToAction(ability, allyId),
        ],
        tag: "event",
        subtag: "general",
      };
      sprite.events.onInputDown.add(() => addToSolution(board, abilityCard));
      sprites.push(sprite);
    }

    x -= 25;
    allyId += 1;
  }

  x = 360;
  for (const enemy of gs.enemies) {
    const sprite: Phaser.Graphics = board.game.add.graphics(x, y, board.group);
    sprite.beginFill(0xCC7744);
    sprite.drawRect(0, 0, 20, 40);
    sprite.endFill();
    sprites.push(sprite);

    const hpRatio = enemy.hp / enemy.maxHp;
    const hpSprite: Phaser.Graphics = board.game.add.graphics(x, y + 45, board.group);
    hpSprite.beginFill(0xFF0000);
    hpSprite.drawRect(0, 0, 20 * hpRatio, 5);
    hpSprite.endFill();
    sprites.push(hpSprite);

    x += 25;
  }

  board.graphics.stateGfx = sprites;
}