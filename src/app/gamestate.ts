import { Solution, runSolution, extendSolution } from "src/shared/game/solution";
import { Card, PlayerOrigin } from "src/shared/game/card";
import { GameState, IdCrew, IdEnemy, CreatureId } from "src/shared/game/state";
import { InputEntityEffect, EntityEffect, solCardFromAbility } from "src/shared/game/ability";
import { Action } from "src/shared/game/action";
import { showTrigger } from "src/shared/game/trigger";
import { HasStatus, showStatus } from "src/shared/game/status";
import { TargetType } from "src/shared/game/target";
import { Ability } from "src/shared/game/crew";
import { InputType } from "../shared/game/input";
import { Context } from "../shared/game/effectvar";
import { Instance } from "src/shared/game/instance";
import { emptyTree, Location } from "src/shared/tree";

export type Limit = {
  limit: number,
};

let targeting: boolean = false;
let targeted: CreatureId | undefined = undefined;

export type LimitedCard = Card & Limit;

export type LeftMenuOption = "crew" | "enemy" | "item" | "general" | "rest";

export const allLeftMenuOptions: LeftMenuOption[] = ["crew", "enemy", "item", "general", "rest"];

export type Board = {
  availableCards: LimitedCard[],
  solution: Solution,
  loc: Location,
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
  treeGfx: Phaser.Graphics[],
  stateGfx: Phaser.Graphics[],
  infoTexts: Phaser.Text[],
};

export function newBoard(
  game: Phaser.Game,
  group: Phaser.Group,
) {
  const board: Board = {
    solution: emptyTree(),
    loc: [],
    availableCards: [],
    selectedLeftMenu: "crew",
    graphics: {
      leftMenuTabs: [],
      availableCardsGfx: [],
      solutionGfx: [],
      treeGfx: [],
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

  const exportBtn: Phaser.Graphics = game.add.graphics(620, 550, group);
  exportBtn.beginFill(0xFFFFFF);
  exportBtn.drawRect(0, 0, 80, 20);
  exportBtn.endFill();
  exportBtn.inputEnabled = true;
  exportBtn.events.onInputDown.add(() => console.log(JSON.stringify(board.solution)));

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
  board.solution = emptyTree();
  board.loc = [];
  mkSolution(board);
}

function changeLoc(
  board: Board,
  loc: Location,
) {
  board.loc = loc;
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
    .filter((card) => card.tag === board.selectedLeftMenu);

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
    for (const action of card.effects) {
      const effect: Phaser.Graphics = board.game.add.graphics(x, y, board.group);
      effect.beginFill(0x66BB88);
      effect.drawRect(0, 0, 165, 20);
      effect.endFill();
      effect.tint = 0xFFFFFF;
      effect.inputEnabled = true;
      effect.events.onInputDown.add(() => console.log(`id: ${JSON.stringify(card.origin)} index ${i}`));
      //effect.events.onInputOver.add(() => showAction(board, action));
      effects.push(effect);
      
      const fontSize = Math.round(Math.min(21, nameBoxWidth / action.description.length) * .70);
      const effectText: Phaser.Text = board.game.add.text(0, 0, action.description, {
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

function cb1(
  effect: InputEntityEffect
): (inputs: any[]) => EntityEffect {
  return (inputs: any[]) => {
    return {
      effect: (context: Context) => { return effect.effect({ ...context, inputs })},
      description: effect.description,
    };
  }
} 

async function addToSolution(
  board: Board,
  card: Card,
) {
  if (card.origin.tag === "PlayerOrigin") {
    const index = board.availableCards.findIndex(c =>
      (<PlayerOrigin>c.origin).cardId === (<PlayerOrigin>card.origin).cardId);
    if (board.availableCards[index].limit <= 0) {
      return "noUses";
    }
    board.availableCards[index].limit -= 1;
  }
  // add user input to card
  let inputs: any[] = [];
  for (const effect of card.effects) {
    inputs = await getUserInputs(board, effect.inputs, inputs);
  }

  // TODO: unify with function from general.ts
  board.solution = extendSolution({...card, inputs}, board.solution, board.loc);
  board.loc = board.loc.concat(0);

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
  // TODO: implement remove from solution
  throw "TODO";
  if (card.origin.tag === "PlayerOrigin") {
    const index = board.availableCards.findIndex(c =>
      (<PlayerOrigin>c.origin).cardId === (<PlayerOrigin>card.origin).cardId);
    board.availableCards[index].limit += 1;
  }
  chLeftMenuTab(board, board.selectedLeftMenu);
  mkSolution(board);
}

function drawTree(
  board: Board,
  solution: Solution,
  loc: Location,
  x: number,
  y: number,
): { x: number, y: number, sprites: Phaser.Graphics[] } {
  let sprites: Phaser.Graphics[] = [];
  let i = 0;
  for (const node of solution.nodes) {
    const newLoc = loc.concat(i);

    const sprite: Phaser.Graphics = board.game.add.graphics(x, y, board.group);
    if (newLoc.toString() === board.loc.toString()) {
      sprite.beginFill(0xFF77CC);
    } else {
      sprite.beginFill(0x4477CC);
    }
    sprite.drawRect(0, 0, 7, 7);
    sprite.endFill();
    sprite.inputEnabled = true;
    sprite.events.onInputDown.add(() => changeLoc(board, newLoc));

    sprites.push(sprite);
    const result = drawTree(board, node.tree, newLoc, x + 10, y);
    sprites = sprites.concat(result.sprites);

    y = result.y + 10;
    i += 1;
  }
  return { x, y, sprites }
}

function mkTree(
  board: Board,
) {
  // clear old
  for (const sprite of board.graphics.treeGfx) {
    sprite.destroy();
  }

  // create new
  const sprites: Phaser.Graphics[] = drawTree(board, board.solution, [], 220, 100).sprites;

  board.graphics.treeGfx = sprites;
}

function mkSolution(
  board: Board,
  // solutionResults: SolutionResult[],
) {
  mkTree(board);

  // update solution

  const result = runSolution(board.solution, board.loc);
  console.log(result.log);
  if (result.state === "invalid") {
    console.log("invalid state");
    board.lastState = undefined;
    clearState(board);
  } else {
    console.log(result.state);
    board.lastState = result.state;
    mkState(board, result.state);
  }
}

function onSolutionEventCardClick(
  board: Board,
  card: Card,
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


function useAbilityCb(
  board: Board,
  ability: InputEntityEffect,
  id: number,
) {
  return function() {
    const card = solCardFromAbility(ability, { tag: "PositionId", id, type: "ally"});
    return addToSolution(board, card); 
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
    sprite.inputEnabled = true;
    sprite.events.onInputOver.add(() => showAlly(board, ally));
    sprite.events.onInputDown.add(() => {
      if (targeting) {
        targeted = {
          tag: "GlobalId",
          id: ally.id,
          type: "ally",
        }
      }
    });
    sprites.push(sprite);

    const hpRatio = ally.hp / ally.maxHp;
    const hpSprite: Phaser.Graphics = board.game.add.graphics(x, y + 45, board.group);
    hpSprite.beginFill(0xFF0000);
    hpSprite.drawRect(0, 0, 20 * hpRatio, 5);
    hpSprite.endFill();
    sprites.push(hpSprite);

    let i = 0;
    for (const ability of ally.abilities) {
      const sprite: Phaser.Graphics = board.game.add.graphics(x, y + 70 + i * 20, board.group);
      sprite.beginFill(0x449966);
      sprite.drawRect(0, 0, 20, 15);
      sprite.endFill();
      sprite.inputEnabled = true;
      sprite.events.onInputOver.add(() => showAbility(board, ability));
      // capture allyId in new scope
      sprite.events.onInputDown.add(useAbilityCb(board, ability, allyId));
      sprites.push(sprite);
      i += 1;
    }

    const statusSprite: Phaser.Graphics = board.game.add.graphics(x, y + 80 + i * 20, board.group);
    statusSprite.beginFill(0x994499);
    statusSprite.drawRect(0, 0, 20, 10);
    statusSprite.endFill();
    statusSprite.inputEnabled = true;
    statusSprite.events.onInputOver.add(() => showEntityStatus(board, ally));
    sprites.push(statusSprite);

    x -= 25;
    allyId += 1;
  }

  x = 330;

  for (const instance of gs.allyInstances) {
    const sprite: Phaser.Graphics = board.game.add.graphics(x, y + 130, board.group);
    sprite.beginFill(0x4477CC);
    sprite.drawRect(0, 0, 10, 20);
    sprite.endFill();
    sprite.inputEnabled = true;
    sprite.events.onInputOver.add(() => showInstance(board, instance));
    sprites.push(sprite);

    x -= 15;
  }


  x = 360;
  for (const enemy of gs.enemies) {
    const sprite: Phaser.Graphics = board.game.add.graphics(x, y, board.group);
    sprite.beginFill(0xCC7744);
    sprite.drawRect(0, 0, 20, 40);
    sprite.endFill();
    sprite.inputEnabled = true;
    sprite.events.onInputOver.add(() => showEnemy(board, enemy));
    sprite.events.onInputDown.add(() => {
      if (targeting) {
        targeted = {
          tag: "GlobalId",
          id: enemy.id,
          type: "enemy",
        }
      }
    });
    sprites.push(sprite);

    const hpRatio = enemy.hp / enemy.maxHp;
    const hpSprite: Phaser.Graphics = board.game.add.graphics(x, y + 45, board.group);
    hpSprite.beginFill(0xFF0000);
    hpSprite.drawRect(0, 0, 20 * hpRatio, 5);
    hpSprite.endFill();
    sprites.push(hpSprite);

    const statusSprite: Phaser.Graphics = board.game.add.graphics(x, y + 75, board.group);
    statusSprite.beginFill(0x994499);
    statusSprite.drawRect(0, 0, 20, 10);
    statusSprite.endFill();
    statusSprite.inputEnabled = true;
    statusSprite.events.onInputOver.add(() => showEntityStatus(board, enemy));
    sprites.push(statusSprite);


    x += 25;
  }

  x = 360;
  for (const instance of gs.enemyInstances) {
    const sprite: Phaser.Graphics = board.game.add.graphics(x, y + 130, board.group);
    sprite.beginFill(0xCC7744);
    sprite.drawRect(0, 0, 10, 20);
    sprite.endFill();
    sprite.inputEnabled = true;
    sprite.events.onInputOver.add(() => showInstance(board, instance));
    sprites.push(sprite);

    x += 15;
  }

  board.graphics.stateGfx = sprites;
}

function waitForTarget(): Promise<CreatureId> {
  return new Promise(function (resolve, reject) {
      (function wait(){
        if (targeted !== undefined) {
          const temp = targeted;
          targeted = undefined;
          targeting = false;
          return resolve(temp);
        }
        setTimeout(wait, 30);
      })();
  });
}

async function getUserInputs(
  board: Board,
  toHandleInputs: InputType[],
  currentInputs: any[],
): Promise<any[]> {
  if (toHandleInputs.length === 0) {
    return currentInputs;
  } else {
    const currentInputType = toHandleInputs[0];
    const tail = toHandleInputs.slice(1);
    switch (currentInputType.tag) {
      case "TargetInput": {
        targeting = true;
        const gid: CreatureId = await waitForTarget();
        return getUserInputs(board, tail, currentInputs.concat(gid));
      }
      case "NumberInput": {
        let input = prompt("Enter Number");
        if (input === null) {
          input = "0";
        }
        const inputParsed = parseInt(input);
        return getUserInputs(board, tail, currentInputs.concat(inputParsed));
      }
      default: {
        throw "getUserInputs: impossible";
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
        const fontSize = 15; // Math.round(Math.min(21, enemyActionWidth / actionShort(enemyAction).length));
        const enemyActionText: Phaser.Text = board.game.add.text(0, 0, enemyAction.description, {
          font: "Arial",
          fontSize: fontSize,
          fill: "#222222",
          boundsAlignH: "center",
          boundsAlignV: "middle",
        }, board.group);
        enemyActionText.setTextBounds(x, y, enemyActionWidth, 22);
        infoTexts.push(enemyActionText);
  
        y += 25;
      }

      y += 60;

      for (const enemyTrigger of action.enemy.triggers) {
        const fontSize = 15; // Math.round(Math.min(21, enemyActionWidth / showTrigger(enemyTrigger).length));
        const enemyTriggerText: Phaser.Text = board.game.add.text(0, 0, enemyTrigger.description, {
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

function showEntityStatus(
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

  for (const status of hasStatus.status) {
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

    y += 25;
  }
  board.graphics.infoTexts = infoTexts;
}

function showAlly(
  board: Board,
  ally: IdCrew,
) {
  // clear old
  for (const text of board.graphics.infoTexts) {
    text.destroy();
  }

  const x = 620;
  let y = 50;
  const infoTexts: Phaser.Text[] = [];

  const fontSize = 15;
  const hpText: Phaser.Text = board.game.add.text(0, 0, `HP: ${ally.hp}/${ally.maxHp} AP: ${ally.ap}`, {
    font: "Arial",
    fontSize: fontSize,
    fill: "#222222",
    boundsAlignH: "center",
    boundsAlignV: "middle",
  }, board.group);
  hpText.setTextBounds(x, y, 180, 22);
  infoTexts.push(hpText);

  y += 40;
  const chargesText: Phaser.Text = board.game.add.text(0, 0, `Charges: ${ally.charges}`, {
    font: "Arial",
    fontSize: fontSize,
    fill: "#222222",
    boundsAlignH: "center",
    boundsAlignV: "middle",
  }, board.group);
  chargesText.setTextBounds(x, y, 180, 22);
  infoTexts.push(chargesText);

  y += 40;
  const threatMapText: Phaser.Text = board.game.add.text(0, 0, `${JSON.stringify(ally.threatMap)}`, {
    font: "Arial",
    fontSize: fontSize,
    fill: "#222222",
    boundsAlignH: "center",
    boundsAlignV: "middle",
  }, board.group);
  threatMapText.setTextBounds(x, y, 180, 22);
  infoTexts.push(threatMapText);

  y += 40;
  const actionText: Phaser.Text = board.game.add.text(0, 0, `Actions`, {
    font: "Arial",
    fontSize: fontSize,
    fill: "#222222",
    boundsAlignH: "center",
    boundsAlignV: "middle",
  }, board.group);
  actionText.setTextBounds(x, y, 180, 22);
  infoTexts.push(actionText);

  for (const action of ally.actions) {
    y += 30;
    const actionDescText: Phaser.Text = board.game.add.text(0, 0, action.description, {
      font: "Arial",
      fontSize: fontSize,
      fill: "#222222",
      boundsAlignH: "center",
      boundsAlignV: "middle",
    }, board.group);
    actionDescText.setTextBounds(x, y, 180, 22);
    infoTexts.push(actionDescText);
  }

  y += 40;
  const triggerText: Phaser.Text = board.game.add.text(0, 0, `Triggers`, {
    font: "Arial",
    fontSize: fontSize,
    fill: "#222222",
    boundsAlignH: "center",
    boundsAlignV: "middle",
  }, board.group);
  triggerText.setTextBounds(x, y, 180, 22);
  infoTexts.push(triggerText);
  
  board.graphics.infoTexts = infoTexts;
}

function showEnemy(
  board: Board,
  enemy: IdEnemy,
) {
  // clear old
  for (const text of board.graphics.infoTexts) {
    text.destroy();
  }

  const x = 620;
  let y = 50;
  const infoTexts: Phaser.Text[] = [];

  const fontSize = 15;
  const enemyActionText: Phaser.Text = board.game.add.text(0, 0, `HP: ${enemy.hp}/${enemy.maxHp}`, {
    font: "Arial",
    fontSize: fontSize,
    fill: "#222222",
    boundsAlignH: "center",
    boundsAlignV: "middle",
  }, board.group);
  enemyActionText.setTextBounds(x, y, 180, 22);
  infoTexts.push(enemyActionText);

  y += 40;
  const chargesText: Phaser.Text = board.game.add.text(0, 0, `Charges: ${enemy.charges}`, {
    font: "Arial",
    fontSize: fontSize,
    fill: "#222222",
    boundsAlignH: "center",
    boundsAlignV: "middle",
  }, board.group);
  chargesText.setTextBounds(x, y, 180, 22);
  infoTexts.push(chargesText);

  y += 40;
  const actionText: Phaser.Text = board.game.add.text(0, 0, `Actions`, {
    font: "Arial",
    fontSize: fontSize,
    fill: "#222222",
    boundsAlignH: "center",
    boundsAlignV: "middle",
  }, board.group);
  actionText.setTextBounds(x, y, 180, 22);
  infoTexts.push(actionText);

  let i = 0;
  for (const action of enemy.actions) {
    y += 30;
    const actionDesc = i === enemy.actionIndex ? `> ${action.description}` : action.description;
    const actionDescText: Phaser.Text = board.game.add.text(0, 0, actionDesc, {
      font: "Arial",
      fontSize: fontSize,
      fill: "#222222",
      boundsAlignH: "center",
      boundsAlignV: "middle",
    }, board.group);
    actionDescText.setTextBounds(x, y, 180, 22);
    infoTexts.push(actionDescText);
    i += 1;
  }

  board.graphics.infoTexts = infoTexts;
}

function showAbility(
  board: Board,
  ability: InputEntityEffect,
) {
  // clear old
  for (const text of board.graphics.infoTexts) {
    text.destroy();
  }

  const x = 620;
  let y = 50;
  const infoTexts: Phaser.Text[] = [];

  const fontSize = 15;
  const enemyActionText: Phaser.Text = board.game.add.text(0, 0, ability.description, {
    font: "Arial",
    fontSize: fontSize,
    fill: "#222222",
    boundsAlignH: "center",
    boundsAlignV: "middle",
  }, board.group);
  enemyActionText.setTextBounds(x, y, 180, 22);
  infoTexts.push(enemyActionText);

  board.graphics.infoTexts = infoTexts;
}

function showInstance(
  board: Board,
  instance: Instance,
) {
  // clear old
  for (const text of board.graphics.infoTexts) {
    text.destroy();
  }

  const x = 620;
  let y = 50;
  const infoTexts: Phaser.Text[] = [];

  const fontSize = 15;
  const hpText: Phaser.Text = board.game.add.text(0, 0, `HP: ${instance.hp}/${instance.maxHp} AP: ${instance.ap}`, {
    font: "Arial",
    fontSize: fontSize,
    fill: "#222222",
    boundsAlignH: "center",
    boundsAlignV: "middle",
  }, board.group);
  hpText.setTextBounds(x, y, 180, 22);
  infoTexts.push(hpText);

  y += 40;
  const actionText: Phaser.Text = board.game.add.text(0, 0, `Actions`, {
    font: "Arial",
    fontSize: fontSize,
    fill: "#222222",
    boundsAlignH: "center",
    boundsAlignV: "middle",
  }, board.group);
  actionText.setTextBounds(x, y, 180, 22);
  infoTexts.push(actionText);

  y += 30;
  const actionDescText: Phaser.Text = board.game.add.text(0, 0, instance.action.description, {
    font: "Arial",
    fontSize: fontSize,
    fill: "#222222",
    boundsAlignH: "center",
    boundsAlignV: "middle",
  }, board.group);
  actionDescText.setTextBounds(x, y, 180, 22);
  infoTexts.push(actionDescText);

  board.graphics.infoTexts = infoTexts;
}