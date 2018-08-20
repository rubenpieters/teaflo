import { Solution, runSolutionAll } from "src/shared/game/solution";
import { Card, Rest, Event } from "src/shared/game/card";
import { GameState } from "src/shared/game/state";
import { createCard } from "src/shared/game/ability";
import { actionShort } from "src/shared/game/log";
import { Action } from "src/shared/game/action";
import { showTrigger } from "src/shared/game/trigger";
import { HasStatus, allStatus, showStatus } from "src/shared/game/status";
import { TargetType } from "src/shared/game/target";
import { Ability } from "src/shared/game/crew";
import { InputType } from "../shared/game/input";

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
  lastState?: GameState,
};

type BoardGraphics = {
  leftMenuTabs: Phaser.Graphics[],
  availableCardsGfx: AvailableCardGfx[],
  solutionGfx: Phaser.Graphics[],
  stateGfx: Phaser.Graphics[],
  infoTexts: Phaser.Text[],
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
      infoTexts: [],
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
  board.graphics.leftMenuTabs.forEach(g => g.tint = 0xFFFFFF);
  board.graphics.leftMenuTabs[allLeftMenuOptions.indexOf(cardType)].tint = 0xAAAAAA;
  board.graphics.availableCardsGfx = popLeftMenu(board);
}

type AvailableCardGfx = {
  title: Phaser.Graphics,
  limitText: Phaser.Text,
  effects: Phaser.Graphics[],
  effectTexts: Phaser.Text[],
  nameText: Phaser.Text,
};

export function chAvailableCards(
  board: Board,
  availableCards: LimitedCard[],
) {
  board.availableCards = availableCards;
  board.graphics.availableCardsGfx = popLeftMenu(board);
}

export function resetSolution(
  board: Board,
) {
  board.solution = { paths: [] };
  mkSolution(board);
}

function popLeftMenu(
  board: Board,
): AvailableCardGfx[] {
  // clear old
  for (const gfx of board.graphics.availableCardsGfx) {
    gfx.title.destroy();
    gfx.limitText.destroy();
    gfx.effects.forEach(x => x.destroy());
    gfx.effectTexts.forEach(x => x.destroy());
    gfx.nameText.destroy();
  }

  const filteredCards = board.availableCards
    .filter((card) => card.subtag === board.selectedLeftMenu);

  const x = 5;
  let y = 80;
  const gfx: AvailableCardGfx[] = [];
  for (const card of filteredCards) {
    const title: Phaser.Graphics = board.game.add.graphics(x, y, board.group);
    title.beginFill(0x449966);
    title.drawRect(0, 0, 165, 30);
    title.endFill();
    title.inputEnabled = true;
    title.events.onInputDown.add(() => addToSolution(board, card));

    const limitContent = Number.isFinite(card.limit) ? card.limit.toString() : "∞";
    const limitText = board.game.add.text(x + 170, y, limitContent, {
      font: "Arial",
      fontSize: 20,
      fill: "#000000",
      boundsAlignH: "center",
      boundsAlignV: "middle"
    }, board.group);
    
    const nameBoxWidth = 165;
    const fontSize = Math.min(21, Math.round(nameBoxWidth / card.name.length));
    const nameText: Phaser.Text = board.game.add.text(0, 0, card.name, {
      font: "Arial",
      fontSize: fontSize,
      fill: "#222222",
      boundsAlignH: "center",
      boundsAlignV: "middle",
    }, board.group);
    nameText.setTextBounds(x, y, nameBoxWidth, 33);

    y += 35;

    const effects: Phaser.Graphics[] = [];
    const effectTexts: Phaser.Text[] = [];
    let i = 0;
    for (const action of card.actions) {
      const effect: Phaser.Graphics = board.game.add.graphics(x, y, board.group);
      effect.beginFill(0x66BB88);
      effect.drawRect(0, 0, 165, 20);
      effect.endFill();
      effect.tint = 0xFFFFFF;
      effect.inputEnabled = true;
      effect.events.onInputDown.add(() => console.log(`id: ${card.id} index ${i}`));
      effect.events.onInputOver.add(() => showAction(board, action));
      effects.push(effect);
      
      const fontSize = Math.round(Math.min(21, nameBoxWidth / actionShort(action).length) * .70);
      const effectText: Phaser.Text = board.game.add.text(0, 0, actionShort(action), {
        font: "Arial",
        fontSize: fontSize,
        fill: "#222222",
        boundsAlignH: "center",
        boundsAlignV: "middle",
      }, board.group);
      effectText.setTextBounds(x, y, nameBoxWidth, 22);
      effectTexts.push(effectText);

      y += 25;
      i += 1;
    }

    gfx.push({ title, limitText, effects, effectTexts, nameText });
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
  for (const path of board.solution.paths) {
    // increase 1 for rest action
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
    board.lastState = undefined;
    clearState(board);
  } else {
    const lastResult = solutionResults[solutionResults.length - 1];
    console.log(lastResult.log);
    if (lastResult.state === "invalid") {
      console.log("invalid state");
      board.lastState = undefined;
      clearState(board);
    } else {
      board.lastState = lastResult.state;
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
    _sprite: Phaser.Sprite,
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
    _sprite: Phaser.Sprite,
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

      sprite.events.onInputDown.add(onAbilityClick(board, ability, allyId, ability.inputs, []));
      sprites.push(sprite);
    }

    const statusSprite: Phaser.Graphics = board.game.add.graphics(x, y + 95, board.group);
    statusSprite.beginFill(0x994499);
    statusSprite.drawRect(0, 0, 20, 10);
    statusSprite.endFill();
    statusSprite.inputEnabled = true;
    statusSprite.events.onInputOver.add(() => showEntityStatus(board, ally));
    sprites.push(statusSprite);

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

function onAbilityClick(
  board: Board,
  ability: Ability,
  allyId: number,
  toHandleInputs: InputType[],
  currentInputs: any[],
) {
  return function() {
    if (toHandleInputs.length === 0) {
      addToSolution(board, createCard(ability.f(currentInputs)(board.lastState!, allyId, "ally"), allyId, "ally"));
    } else {
      const currentInputType = toHandleInputs[0];
      const tail = toHandleInputs.slice(1, toHandleInputs.length);
      switch (currentInputType.tag) {
        case "TargetInput": {
          throw "TODO";
        }
        case "NumberInput": {
          let input = prompt("Enter Number");
          if (input === null) {
            input = "0";
          }
          const inputParsed = parseInt(input);
          onAbilityClick(board, ability, allyId, tail, currentInputs.concat(inputParsed));
        }
      }
    }
  }
}

function showAction(
  board: Board,
  action: Action,
) {
  // clear old
  for (const text of board.graphics.infoTexts) {
    text.destroy();
  }

  const x = 620;
  let y = 50;
  const infoTexts: Phaser.Text[] = [];

  switch (action.tag) {
    case "AddEnemy": {
      const hpBoxWidth = 100;
      const hpTextContent = `HP ${action.enemy.hp}/${action.enemy.maxHp}`;
      const fontSize = Math.round(Math.min(21, hpBoxWidth / hpTextContent.length) * .70);
      const hpText: Phaser.Text = board.game.add.text(0, 0, hpTextContent, {
        font: "Arial",
        fontSize: fontSize,
        fill: "#222222",
        boundsAlignH: "center",
        boundsAlignV: "middle",
      }, board.group);
      hpText.setTextBounds(x, y, hpBoxWidth, 22);
      infoTexts.push(hpText);

      y += 30;

      const enemyActionWidth = 180;
      for (const enemyAction of action.enemy.actions) {
        /*const fontSize = 15; // Math.round(Math.min(21, enemyActionWidth / actionShort(enemyAction).length));
        const enemyActionText: Phaser.Text = board.game.add.text(0, 0, actionShort(enemyAction), {
          font: "Arial",
          fontSize: fontSize,
          fill: "#222222",
          boundsAlignH: "center",
          boundsAlignV: "middle",
        }, board.group);
        enemyActionText.setTextBounds(x, y, enemyActionWidth, 22);
        infoTexts.push(enemyActionText);*/
  
        y += 25;
      }

      y += 60;

      for (const enemyTrigger of action.enemy.triggers) {
        const fontSize = 15; // Math.round(Math.min(21, enemyActionWidth / showTrigger(enemyTrigger).length));
        const enemyTriggerText: Phaser.Text = board.game.add.text(0, 0, showTrigger(enemyTrigger), {
          font: "Arial",
          fontSize: fontSize,
          fill: "#222222",
          boundsAlignH: "center",
          boundsAlignV: "middle",
        }, board.group);
        enemyTriggerText.setTextBounds(x, y, enemyActionWidth, 22);
        infoTexts.push(enemyTriggerText);

        y += 25;
      }
    }
  }

  board.graphics.infoTexts = infoTexts;
}

function showEntityStatus (
  board: Board,
  hasStatus: HasStatus,
) {
  // clear old
  for (const text of board.graphics.infoTexts) {
    text.destroy();
  }

  const x = 620;
  let y = 50;
  const infoTexts: Phaser.Text[] = [];

  for (const statusType of allStatus) {
    const status = hasStatus[statusType];
    if (status !== undefined) {
      const fontSize = 15;
      const enemyActionText: Phaser.Text = board.game.add.text(0, 0, showStatus(status), {
        font: "Arial",
        fontSize: fontSize,
        fill: "#222222",
        boundsAlignH: "center",
        boundsAlignV: "middle",
      }, board.group);
      enemyActionText.setTextBounds(x, y, 180, 22);
      infoTexts.push(enemyActionText);
    }

    y += 25;
  }
  board.graphics.infoTexts = infoTexts;
}