import * as allCrew from "src/shared/data/crew";
import * as allItems from "src/shared/data/item";
import * as allEnemies from "src/shared/data/enemy";
import { GameState, IdCrew, CreatureId } from "src/shared/game/state";
import { Card, Rest, Event } from "src/shared/game/card";

export const cardRest: Rest = {
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

export const cardBattleTurn: Event = {
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
const cardCrew_0000: Event = {
  id: 1000,
  name: "Fighter",
  actions: [{ tag: "AddCrew", crew: allCrew.stFighter }],
  tag: "event",
  subtag: "crew",
};

const cardCrew_0001: Event = {
  id: 1001,
  name: "Archer",
  actions: [{ tag: "AddCrew", crew: allCrew.stRanged }],
  tag: "event",
  subtag: "crew",
};

const cardCrew_0002: Event = {
  id: 1002,
  name: "Healer",
  actions: [
    { tag: "AddCrew", crew: allCrew.abilityHeal },
  ],
  tag: "event",
  subtag: "crew",
};
*/

export const cardCrew_0003: Event = {
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

export const cardCrew_0004: Event = {
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

export const cardCrew_0005: Event = {
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

export const cardCrew_0006: Event = {
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

export const cardCrew_0007: Event = {
  origin: { tag: "PlayerOrigin", cardId: 1007 },
  name: "Dmg2",
  effects: [
    { effect: (_inputs: any[]) => { return (state: GameState, _id: CreatureId) => {
      return allCrew.onAllAlly(state, 
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

export const cardCrew_0008: Event = {
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

export const cardCrew_0009: Event = {
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

export const cardCrew_0010: Event = {
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

export const cardCrew_0011: Event = {
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

export const cardCrew_0012: Event = {
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

export const cardCrew_0013: Event = {
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

export const cardCrew_0014: Event = {
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

export const cardItem_0000: Event = {
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
const cardBattle_0000: Event = {
  id: 3000,
  name: "Enemy0",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemyAtk012 },
    { tag: "GainGold", gain: 10 },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0001: Event = {
  id: 3001,
  name: "Enemy1",
  actions: [
    // TODO: { tag: "AddEnemy", enemy: allEnemies.enemyHeal2R14 },
    { tag: "GainGold", gain: 7 },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0002: Event = {
  id: 3002,
  name: "Enemy2",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemyRegenApMinR20 },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0003: Event = {
  id: 3003,
  name: "Enemy3",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemy8HpAtk2 },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0004: Event = {
  id: 3004,
  name: "Enemy4",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemy15hpAtk1AllHeal2 },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0005: Event = {
  id: 3005,
  name: "Enemy5",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemy14hpApMin },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0006: Event = {
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

const cardBattle_0007: Event = {
  id: 3007,
  name: "Enemy7",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemy20HpDoom },
  ],
  tag: "event",
  subtag: "enemy",
};

const cardBattle_0008: Event = {
  id: 3008,
  name: "Enemy8",
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemyAtk2Hp15 },
  ],
  tag: "event",
  subtag: "enemy",
};*/

export const cardBattle_0009: Event = {
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

export const cardBattle_0010: Event = {
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

export const cardBattle_0011: Event = {
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

export const cardBattle_0012: Event = {
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

export const cardBattle_0013: Event = {
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

export const cardBattle_0014: Event = {
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