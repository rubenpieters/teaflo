import { focus, over, set } from "src/shared/iassign-util";
import { Solution } from "src/shared/game/solution";
import { Card, Rest, Event } from "src/shared/game/card";

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
    title.events.onInputDown.add(() => console.log("id: " + card.id));
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