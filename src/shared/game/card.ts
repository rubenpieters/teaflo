import { Action, ActionSpec } from "src/shared/game/action";
import { Target, Origin, TargetType } from "src/shared/game/target";
import { allCrew, onAllAlly, Ability } from "src/shared/game/crew";
import { allItems } from "src/shared/game/item";
import { allEnemies } from "src/shared/game/enemy";
import { showAction } from "src/shared/game/log";
import { GameState, IdCrew } from "./state";

export function showCard(card: Card) {
  //return { ...card, actions: card.actions.map(showAction) };
}

export type PlayerOrigin = {
  tag: "PlayerOrigin",
  cardId: number,
};

export type EntityOrigin = {
  tag: "EntityOrigin",
  entityId: number,
  entityType: TargetType,
};

export type CardOrigin
  = PlayerOrigin
  | EntityOrigin
  ;

export type Event = {
  tag: "crew" | "enemy" | "item" | "general",
  name: string,
  origin: CardOrigin,
  effects: Ability[],
};

export type Rest = {
  tag: "rest",
  name: string,
  origin: CardOrigin,
  effects: Ability[],
};

export type Card = Event | Rest;

const cardRest: Card = {
  origin: { tag: "PlayerOrigin", cardId: 0 },
  name: "Rest",
  effects: [
    { f: (_inputs: any[]) => { return (_state: GameState, _id: number, _type: TargetType) => {
      return { tag: "Rest" }
      }},
      inputs: [],
    },
  ],
  tag: "rest",
};

const cardBattleTurn: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1 },
  name: "Battle",
  effects: [
    { f: (_inputs: any[]) => { return (_state: GameState, _id: number, _type: TargetType) => {
      return { tag: "BattleTurn" }
      }},
      inputs: [],
    },
  ],
  tag: "general",
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
  origin: { tag: "PlayerOrigin", cardId: 1003 },
  name: "ArmorSelf",
  effects: [
    { f: (_inputs: any[]) => { return (_state: GameState, _id: number, _type: TargetType) => {
      return { tag: "AddCrew", crew: allCrew.armorOnSelfHeal };
      }},
      inputs: [],
    },
  ],
  tag: "crew",
};

const cardCrew_0004: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1004 },
  name: "RegenDmg",
  effects: [
    { f: (_inputs: any[]) => { return (_state: GameState, _id: number, _type: TargetType) => {
      return { tag: "AddCrew", crew: allCrew.regenOnDamageAlly };
      }},
      inputs: [],
    },
  ],
  tag: "crew",
};

const cardCrew_0005: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1005 },
  name: "Tank1",
  effects: [
    { f: (_inputs: any[]) => { return (_state: GameState, _id: number, _type: TargetType) => {
      return { tag: "AddCrew", crew: allCrew.tank1 };
      }},
      inputs: [],
    },
  ],
  tag: "crew",
};

const cardCrew_0006: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1006 },
  name: "Dmg1",
  effects: [
    { f: (_inputs: any[]) => { return (_state: GameState, _id: number, _type: TargetType) => {
      return { tag: "AddCrew", crew: allCrew.dmg1 };
      }},
      inputs: [],
    },
  ],
  tag: "crew",
};

const cardCrew_0007: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1007 },
  name: "Dmg2",
  effects: [
    { f: (_inputs: any[]) => { return (state: GameState, _id: number, _type: TargetType) => {
      return onAllAlly(state, 
        (ally: IdCrew, id: number) => {
          return {
            tag: "QueueStatus",
            target: { tag: "Target", type: "ally", position: id, },
            status: {
              tag: "Doom",
              value: 0,
              fragment: 50,
            }
          };
        }
      );
      }},
      inputs: [],
    },
    { f: (_inputs: any[]) => { return (_state: GameState, _id: number, _type: TargetType) => {
      return { tag: "AddCrew", crew: allCrew.dmg1 };
      }},
      inputs: [],
    },
  ],
  tag: "crew",
};

const cardItem_0000: Card = {
  origin: { tag: "PlayerOrigin", cardId: 2000 },
  name: "Shield",
  effects: [
    { f: (_inputs: any[]) => { return (_state: GameState, _id: number, _type: TargetType) => {
      return { tag: "PayGold", pay: 5 };
      }},
      inputs: [],
    },
    { f: (_inputs: any[]) => { return (_state: GameState, _id: number, _type: TargetType) => {
      return { tag: "AddItem", item: allItems.guard1StartCombat };
      }},
      inputs: [],
    },
  ],
  tag: "item",
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
  origin: { tag: "PlayerOrigin", cardId: 3009 },
  name: "Enemy9",
  effects: [
    { f: (_inputs: any[]) => { return (_state: GameState, _id: number, _type: TargetType) => {
      return { tag: "AddEnemy", enemy: allEnemies.enemyBoss1 };
      }},
      inputs: [],
    },
  ],
  tag: "enemy",
};

const cardBattle_0010: Card = {
  origin: { tag: "PlayerOrigin", cardId: 3010 },
  name: "Enemy10",
  effects: [
    { f: (_inputs: any[]) => { return (_state: GameState, _id: number, _type: TargetType) => {
      return { tag: "AddEnemy", enemy: allEnemies.enemyBoss2 };
      }},
      inputs: [],
    },
  ],
  tag: "enemy",
};

export const allCards = {
  cardBattleTurn: cardBattleTurn,
  cardRest: cardRest,

  /*cardCrew_0000: cardCrew_0000,
  cardCrew_0001: cardCrew_0001,
  cardCrew_0002: cardCrew_0002,*/
  cardCrew_0003: cardCrew_0003,
  cardCrew_0004: cardCrew_0004,
  cardCrew_0005: cardCrew_0005,
  cardCrew_0006: cardCrew_0006,
  cardCrew_0007: cardCrew_0007,

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
  cardBattle_0010: cardBattle_0010,
};