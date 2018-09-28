import { TargetType } from "src/shared/game/target";
import { allCrew, onAllAlly } from "src/shared/game/crew";
import { allItems } from "src/shared/game/item";
import { allEnemies } from "src/shared/game/enemy";
import { GameState, IdCrew, CreatureId } from "src/shared/game/state";
import { InputEntityEffect } from "src/shared/game/ability";

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
  effects: InputEntityEffect[],
};

export type Rest = {
  tag: "rest",
  name: string,
  origin: CardOrigin,
  effects: InputEntityEffect[],
};

export type Card = Event | Rest;

const cardRest: Card = {
  origin: { tag: "PlayerOrigin", cardId: 0 },
  name: "Rest",
  effects: [
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "Rest" }
      }},
      inputs: [],
      description: "Rest",
    },
  ],
  tag: "rest",
};

const cardBattleTurn: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1 },
  name: "Battle",
  effects: [
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "BattleTurn" }
      }},
      inputs: [],
      description: "BattleTurn",
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
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddCrew", crew: allCrew.armorOnSelfHeal };
      }},
      inputs: [],
      description: "add ArmorSelf",
    },
  ],
  tag: "crew",
};

const cardCrew_0004: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1004 },
  name: "RegenDmg",
  effects: [
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddCrew", crew: allCrew.regenOnDamageAlly };
      }},
      inputs: [],
      description: "add RegenDmg",
    },
  ],
  tag: "crew",
};

const cardCrew_0005: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1005 },
  name: "Tank1",
  effects: [
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddCrew", crew: allCrew.tank1 };
      }},
      inputs: [],
      description: "add Tank1",
    },
  ],
  tag: "crew",
};

const cardCrew_0006: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1006 },
  name: "Dmg1",
  effects: [
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddCrew", crew: allCrew.dmg1 };
      }},
      inputs: [],
      description: "add Dmg1",
    },
  ],
  tag: "crew",
};

const cardCrew_0007: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1007 },
  name: "Dmg2",
  effects: [
    { effect: (_inputs: any[]) => { return (state: GameState, _id: CreatureId) => {
      return onAllAlly(state, 
        (_ally: IdCrew, id: number) => {
          return {
            tag: "QueueStatus",
            target: { tag: "PositionId", type: "ally", id: id, },
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
      description: "doom 0/50 all allies",
    },
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddCrew", crew: allCrew.dmg1 };
      }},
      inputs: [],
      description: "add Dmg1",
    },
  ],
  tag: "crew",
};

const cardCrew_0008: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1008 },
  name: "DmgPoison",
  effects: [
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddCrew", crew: allCrew.dmgPoison };
      }},
      inputs: [],
      description: "add DmgPoison",
    },
  ],
  tag: "crew",
};

const cardCrew_0009: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1009 },
  name: "BasicCrew1",
  effects: [
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddCrew", crew: allCrew.basicCrew1 };
      }},
      inputs: [],
      description: "add BasicCrew1",
    },
  ],
  tag: "crew",
};

const cardCrew_0010: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1010 },
  name: "BasicCrew2",
  effects: [
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddCrew", crew: allCrew.basicCrew2 };
      }},
      inputs: [],
      description: "add BasicCrew2",
    },
  ],
  tag: "crew",
};

const cardCrew_0011: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1011 },
  name: "BasicCrew3",
  effects: [
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddCrew", crew: allCrew.basicCrew3 };
      }},
      inputs: [],
      description: "add BasicCrew3",
    },
  ],
  tag: "crew",
};

const cardCrew_0012: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1012 },
  name: "Tank01",
  effects: [
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddCrew", crew: allCrew.tank_01 };
      }},
      inputs: [],
      description: "add Tank01",
    },
  ],
  tag: "crew",
};

const cardCrew_0013: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1013 },
  name: "Dmg01",
  effects: [
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddCrew", crew: allCrew.dmg_01 };
      }},
      inputs: [],
      description: "add Dmg01",
    },
  ],
  tag: "crew",
};

const cardCrew_0014: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1014 },
  name: "Util01",
  effects: [
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddCrew", crew: allCrew.util_01 };
      }},
      inputs: [],
      description: "add Util01",
    },
  ],
  tag: "crew",
};

const cardItem_0000: Card = {
  origin: { tag: "PlayerOrigin", cardId: 2000 },
  name: "Shield",
  effects: [
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "PayGold", pay: 5 };
      }},
      inputs: [],
      description: "pay 5",
    },
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddItem", item: allItems.guard1StartCombat };
      }},
      inputs: [],
      description: "add item",
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
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddEnemy", enemy: allEnemies.enemyBoss1 };
      }},
      inputs: [],
      description: "add Enemy9",
    },
  ],
  tag: "enemy",
};

const cardBattle_0010: Card = {
  origin: { tag: "PlayerOrigin", cardId: 3010 },
  name: "Enemy10",
  effects: [
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddEnemy", enemy: allEnemies.enemyBoss2 };
      }},
      inputs: [],
      description: "add Enemy10",
    },
  ],
  tag: "enemy",
};

const cardBattle_0011: Card = {
  origin: { tag: "PlayerOrigin", cardId: 3011 },
  name: "Dummy",
  effects: [
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddEnemy", enemy: allEnemies.dummy };
      }},
      inputs: [],
      description: "add Dummy",
    },
  ],
  tag: "enemy",
};

const cardBattle_0012: Card = {
  origin: { tag: "PlayerOrigin", cardId: 3012 },
  name: "BasicEnemy1",
  effects: [
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddEnemy", enemy: allEnemies.basicEnemy1 };
      }},
      inputs: [],
      description: "add BasicEnemy1",
    },
  ],
  tag: "enemy",
};

const cardBattle_0013: Card = {
  origin: { tag: "PlayerOrigin", cardId: 3013 },
  name: "BasicEnemy2",
  effects: [
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddEnemy", enemy: allEnemies.basicEnemy2 };
      }},
      inputs: [],
      description: "add BasicEnemy2",
    },
  ],
  tag: "enemy",
};

const cardBattle_0014: Card = {
  origin: { tag: "PlayerOrigin", cardId: 3014 },
  name: "Boss3",
  effects: [
    { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
      return { tag: "AddEnemy", enemy: allEnemies.enemyBoss3 };
      }},
      inputs: [],
      description: "add Boss3",
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
  cardCrew_0008: cardCrew_0008,
  cardCrew_0009: cardCrew_0009,
  cardCrew_0010: cardCrew_0010,
  cardCrew_0011: cardCrew_0011,
  cardCrew_0012: cardCrew_0012,
  cardCrew_0013: cardCrew_0013,
  cardCrew_0014: cardCrew_0014,

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
  cardBattle_0011: cardBattle_0011,
  cardBattle_0012: cardBattle_0012,
  cardBattle_0013: cardBattle_0013,
  cardBattle_0014: cardBattle_0014,
};