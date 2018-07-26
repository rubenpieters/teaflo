import { Action } from "src/shared/game/action";
import { Target } from "src/shared/game/target";
import { allCrew } from "src/shared/game/crew";
import { allItems } from "src/shared/game/item";
import { allEnemies } from "src/shared/game/enemy";
import { showAction } from "src/shared/game/log";

export function showCard(card: Card) {
  return { ...card, actions: card.actions.map(showAction) };
}

export type Event = {
  tag: "event",
  id: number,
  actions: Action<Target>[],
};

export type Rest = {
  tag: "rest",
  id: number,
  actions: Action<Target>[],
};

export type Card
  = Event
  | Rest
  ;

const cardBattleTurn: Card = {
  id: 0,
  actions: [{ tag: "BattleTurn" }],
  tag: "event"
};

const cardCrew_0000: Card = {
  id: 1,
  actions: [{ tag: "AddCrew", crew: allCrew.stFighter }],
  tag: "event",
};
const cardCrew_0001: Card = {
  id: 2,
  actions: [{ tag: "AddCrew", crew: allCrew.stRanged }],
  tag: "event",
};

const cardItem_0000: Card = {
  id: 3,
  actions: [
    { tag: "PayGold", pay: 5 },
    { tag: "AddItem", item: allItems.guard1StartCombat },
  ],
  tag: "event",
};

const cardBattle_0000: Card = {
  id: 4,
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemyAtk012 },
    { tag: "GainGold", gain: 10 },
  ],
  tag: "event",
};

const cardBattle_0001: Card = {
  id: 5,
  actions: [
    // TODO: { tag: "AddEnemy", enemy: allEnemies.enemyHeal2R14 },
    { tag: "GainGold", gain: 7 },
  ],
  tag: "event",
};

const cardBattle_0002: Card = {
  id: 6,
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemyRegenApMinR20 },
  ],
  tag: "event",
};

const cardBattle_0003: Card = {
  id: 7,
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemy8HpAtk2 },
  ],
  tag: "event",
};

const cardBattle_0004: Card = {
  id: 8,
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemy15hpAtk1AllHeal2 },
  ],
  tag: "event",
};

const cardBattle_0005: Card = {
  id: 9,
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemy15hpAtk1AllHeal2 },
  ],
  tag: "event",
};

const cardBattle_0006: Card = {
  id: 10,
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemy5HpAtkInFront },
    { tag: "AddEnemy", enemy: allEnemies.enemy5HpAtkInFront },
    { tag: "AddEnemy", enemy: allEnemies.enemy5HpAtkInFront },
    { tag: "AddEnemy", enemy: allEnemies.enemy5HpAtkInFront },
  ],
  tag: "event",
};

const cardBattle_0007: Card = {
  id: 11,
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemy20HpDoom },
  ],
  tag: "event",
};

export const allCards = {
  cardBattleTurn: cardBattleTurn,

  cardCrew_0000: cardCrew_0000,
  cardCrew_0001: cardCrew_0001,

  cardItem_0000: cardItem_0000,

  cardBattle_0000: cardBattle_0000,
  cardBattle_0001: cardBattle_0001,
  cardBattle_0002: cardBattle_0002,
  cardBattle_0003: cardBattle_0003,
  cardBattle_0004: cardBattle_0004,
  cardBattle_0005: cardBattle_0005,
  cardBattle_0006: cardBattle_0006,
  cardBattle_0007: cardBattle_0007,
};