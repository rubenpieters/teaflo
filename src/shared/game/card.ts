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
  name: string,
  subtag: "crew" | "enemy" | "item" | "general" | "rest",
  id: number | "created",
  actions: Action<Target>[],
};

export type Rest = {
  tag: "rest",
  name: string,
  subtag: "crew" | "enemy" | "item" | "general" | "rest",
  id: number | "created",
  actions: Action<Target>[],
};

export type Card
  = Event
  | Rest
  ;

const cardRest: Card = {
  id: 0,
  name: "Rest",
  actions: [{ tag: "Rest" }],
  tag: "rest",
  subtag: "rest",
};

const cardBattleTurn: Card = {
  id: 0,
  name: "Battle",
  actions: [{ tag: "BattleTurn" }],
  tag: "event",
  subtag: "general",
};

const cardCrew_0000: Card = {
  id: 1,
  name: "Fighter",
  actions: [{ tag: "AddCrew", crew: allCrew.stFighter }],
  tag: "event",
  subtag: "crew",
};

const cardCrew_0001: Card = {
  id: 2,
  name: "Archer",
  actions: [{ tag: "AddCrew", crew: allCrew.stRanged }],
  tag: "event",
  subtag: "crew",
};

const cardCrew_0002: Card = {
  id: 13,
  name: "Healer",
  actions: [
    { tag: "AddCrew", crew: allCrew.abilityHeal },
  ],
  tag: "event",
  subtag: "crew",
};

const cardItem_0000: Card = {
  id: 3,
  name: "Shield",
  actions: [
    { tag: "PayGold", pay: 5 },
    { tag: "AddItem", item: allItems.guard1StartCombat },
  ],
  tag: "event",
  subtag: "item",
};

const cardBattle_0000: Card = {
  id: 4,
  name: "Enemy0",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemyAtk012 },
    { tag: "GainGold", gain: 10 },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0001: Card = {
  id: 5,
  name: "Enemy1",
  actions: [
    // TODO: { tag: "AddEnemy", enemy: allEnemies.enemyHeal2R14 },
    { tag: "GainGold", gain: 7 },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0002: Card = {
  id: 6,
  name: "Enemy2",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemyRegenApMinR20 },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0003: Card = {
  id: 7,
  name: "Enemy3",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemy8HpAtk2 },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0004: Card = {
  id: 8,
  name: "Enemy4",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemy15hpAtk1AllHeal2 },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0005: Card = {
  id: 9,
  name: "Enemy5",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemy14hpApMin },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0006: Card = {
  id: 10,
  name: "Enemy6",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemy5HpAtkInFront },
    { tag: "AddEnemy", enemy: allEnemies.enemy5HpAtkInFront },
    { tag: "AddEnemy", enemy: allEnemies.enemy5HpAtkInFront },
    { tag: "AddEnemy", enemy: allEnemies.enemy5HpAtkInFront },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0007: Card = {
  id: 11,
  name: "Enemy7",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemy20HpDoom },
  ],
  tag: "event",
  subtag: "enemy",
};


const cardBattle_0008: Card = {
  id: 12,
  name: "Enemy8",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemyAtk2Hp15 },
  ],
  tag: "event",
  subtag: "enemy",
};



export const allCards = {
  cardBattleTurn: cardBattleTurn,
  cardRest: cardRest,

  cardCrew_0000: cardCrew_0000,
  cardCrew_0001: cardCrew_0001,
  cardCrew_0002: cardCrew_0002,

  cardItem_0000: cardItem_0000,

  cardBattle_0000: cardBattle_0000,
  cardBattle_0001: cardBattle_0001,
  cardBattle_0002: cardBattle_0002,
  cardBattle_0003: cardBattle_0003,
  cardBattle_0004: cardBattle_0004,
  cardBattle_0005: cardBattle_0005,
  cardBattle_0006: cardBattle_0006,
  cardBattle_0007: cardBattle_0007,
  cardBattle_0008: cardBattle_0008,
};