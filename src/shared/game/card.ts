import { Action, ActionSpec } from "src/shared/game/action";
import { Target, Origin, TargetType } from "src/shared/game/target";
import { allCrew, Ability } from "src/shared/game/crew";
import { allItems } from "src/shared/game/item";
import { allEnemies } from "src/shared/game/enemy";
import { showAction } from "src/shared/game/log";
import { GameState } from "./state";

export function showCard(card: Card) {
  //return { ...card, actions: card.actions.map(showAction) };
}

export type Event = {
  tag: "event",
  name: string,
  subtag: "crew" | "enemy" | "item" | "general" | "rest",
  id: number | "created",
  actions: Ability[],
  origin?: Origin,
};

export type Rest = {
  tag: "rest",
  name: string,
  subtag: "crew" | "enemy" | "item" | "general" | "rest",
  id: number | "created",
  actions: Ability[],
};

export type Card
  = Event
  | Rest
  ;

const cardRest: Card = {
  id: 0,
  name: "Rest",
  actions: [
    { f: (inputs: any[]) => { return (state: GameState, id: number, type: TargetType) => {
      return { tag: "Rest" }
      }},
      inputs: [],
    },
  ],
  tag: "rest",
  subtag: "rest",
};

const cardBattleTurn: Card = {
  id: 1,
  name: "Battle",
  actions: [
    { f: (inputs: any[]) => { return (state: GameState, id: number, type: TargetType) => {
      return { tag: "BattleTurn" }
      }},
      inputs: [],
    },
  ],
  tag: "event",
  subtag: "general",
};

/*
const cardCrew_0000: Card = {
  id: 1000,
  name: "Fighter",
  actions: [{ tag: "AddCrew", crew: allCrew.stFighter }],
  tag: "event",
  subtag: "crew",
};

const cardCrew_0001: Card = {
  id: 1001,
  name: "Archer",
  actions: [{ tag: "AddCrew", crew: allCrew.stRanged }],
  tag: "event",
  subtag: "crew",
};

const cardCrew_0002: Card = {
  id: 1002,
  name: "Healer",
  actions: [
    { tag: "AddCrew", crew: allCrew.abilityHeal },
  ],
  tag: "event",
  subtag: "crew",
};
*/

const cardCrew_0003: Card = {
  id: 1003,
  name: "ArmorSelf",
  actions: [
    { f: (inputs: any[]) => { return (state: GameState, id: number, type: TargetType) => {
      return { tag: "AddCrew", crew: allCrew.armorOnSelfHeal };
      }},
      inputs: [],
    },
  ],
  tag: "event",
  subtag: "crew",
};

const cardCrew_0004: Card = {
  id: 1004,
  name: "RegenDmg",
  actions: [
    { f: (inputs: any[]) => { return (state: GameState, id: number, type: TargetType) => {
      return { tag: "AddCrew", crew: allCrew.regenOnDamageAlly };
      }},
      inputs: [],
    },
  ],
  tag: "event",
  subtag: "crew",
};

const cardItem_0000: Card = {
  id: 2000,
  name: "Shield",
  actions: [
    { f: (inputs: any[]) => { return (state: GameState, id: number, type: TargetType) => {
      return { tag: "PayGold", pay: 5 };
      }},
      inputs: [],
    },
    { f: (inputs: any[]) => { return (state: GameState, id: number, type: TargetType) => {
      return { tag: "AddItem", item: allItems.guard1StartCombat };
      }},
      inputs: [],
    },
  ],
  tag: "event",
  subtag: "item",
};

/*
const cardBattle_0000: Card = {
  id: 3000,
  name: "Enemy0",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemyAtk012 },
    { tag: "GainGold", gain: 10 },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0001: Card = {
  id: 3001,
  name: "Enemy1",
  actions: [
    // TODO: { tag: "AddEnemy", enemy: allEnemies.enemyHeal2R14 },
    { tag: "GainGold", gain: 7 },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0002: Card = {
  id: 3002,
  name: "Enemy2",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemyRegenApMinR20 },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0003: Card = {
  id: 3003,
  name: "Enemy3",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemy8HpAtk2 },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0004: Card = {
  id: 3004,
  name: "Enemy4",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemy15hpAtk1AllHeal2 },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0005: Card = {
  id: 3005,
  name: "Enemy5",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemy14hpApMin },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0006: Card = {
  id: 3006,
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
  id: 3007,
  name: "Enemy7",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemy20HpDoom },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0008: Card = {
  id: 3008,
  name: "Enemy8",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemyAtk2Hp15 },
  ],
  tag: "event",
  subtag: "enemy",
};*/

const cardBattle_0009: Card = {
  id: 3009,
  name: "Enemy9",
  actions: [
    { f: (inputs: any[]) => { return (state: GameState, id: number, type: TargetType) => {
      return { tag: "AddEnemy", enemy: allEnemies.enemyBoss1 };
      }},
      inputs: [],
    },
  ],
  tag: "event",
  subtag: "enemy",
};

export const allCards = {
  cardBattleTurn: cardBattleTurn,
  cardRest: cardRest,

  /*cardCrew_0000: cardCrew_0000,
  cardCrew_0001: cardCrew_0001,
  cardCrew_0002: cardCrew_0002,*/
  cardCrew_0003: cardCrew_0003,
  cardCrew_0004: cardCrew_0004,

  cardItem_0000: cardItem_0000,

  /*cardBattle_0000: cardBattle_0000,
  cardBattle_0001: cardBattle_0001,
  cardBattle_0002: cardBattle_0002,
  cardBattle_0003: cardBattle_0003,
  cardBattle_0004: cardBattle_0004,
  cardBattle_0005: cardBattle_0005,
  cardBattle_0006: cardBattle_0006,
  cardBattle_0007: cardBattle_0007,
  cardBattle_0008: cardBattle_0008,*/
  cardBattle_0009: cardBattle_0009,
};