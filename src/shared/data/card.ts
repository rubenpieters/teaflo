import * as allCrew from "src/shared/data/crew";
import * as allItems from "src/shared/data/item";
import * as allEnemies from "src/shared/data/enemy";
import { GameState, IdCrew, CreatureId } from "src/shared/game/state";
import { Card } from "src/shared/game/card";

export const cardBattleTurn: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1 },
  name: "Battle",
  effects: [
    { 
      effect: (_obj) => { return { action: { tag: "BattleTurn" }}},
      inputs: [],
      description: "BattleTurn",
    },
  ],
  tag: "general",
};

export const cardDummy: Card = {
  origin: { tag: "PlayerOrigin", cardId: 2 },
  name: "Dummy",
  effects: [
    { effect: (_obj) => {
      return { action: { tag: "AddEnemy", enemy: allEnemies.dummy }};
      },
      inputs: [],
      description: "add Dummy",
    },
  ],
  tag: "enemy",
};

export const cardDummyDmg1: Card = {
  origin: { tag: "PlayerOrigin", cardId: 2 },
  name: "DummyDmg1",
  effects: [
    { effect: (_obj) => {
      return { action: { tag: "AddEnemy", enemy: allEnemies.dummyDmg1 }};
      },
      inputs: [],
      description: "add DummyDmg1",
    },
  ],
  tag: "enemy",
};

export const cardCrew_0001: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1001 },
  name: "Tank01",
  effects: [
    { effect: (_obj) => {
      return { action: { tag: "AddCrew", crew: allCrew.tank_01 }};
      },
      inputs: [],
      description: "add Tank01",
    },
  ],
  tag: "crew",
};

export const cardCrew_0002: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1002 },
  name: "DmgPoison",
  effects: [
    { effect: (_obj) => {
      return { action: { tag: "AddCrew", crew: allCrew.dmgPoison }};
      },
      inputs: [],
      description: "add DmgPoison",
    },
  ],
  tag: "crew",
};

export const cardCrew_0005: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1003 },
  name: "Dmg02",
  effects: [
    { effect: (_obj) => {
      return { action: { tag: "AddCrew", crew: allCrew.dmg_02 }};
      },
      inputs: [],
      description: "add Dmg02",
    },
  ],
  tag: "crew",
};

export const cardCrew_0006: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1003 },
  name: "Dmg03",
  effects: [
    { effect: (_obj) => {
      return { action: { tag: "AddCrew", crew: allCrew.dmg_03 }};
      },
      inputs: [],
      description: "add Dmg03",
    },
  ],
  tag: "crew",
};

export const cardCrew_0008: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1003 },
  name: "Dmg04",
  effects: [
    { effect: (_obj) => {
      return { action: { tag: "AddCrew", crew: allCrew.dmg_04 }};
      },
      inputs: [],
      description: "add Dmg04",
    },
  ],
  tag: "crew",
};

export const cardCrew_0003: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1002 },
  name: "Tank02",
  effects: [
    { effect: (_obj) => {
      return { action: { tag: "AddCrew", crew: allCrew.tank_02 }};
      },
      inputs: [],
      description: "add Tank02",
    },
  ],
  tag: "crew",
};

export const cardCrew_0004: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1003 },
  name: "Tank03",
  effects: [
    { effect: (_obj) => {
      return { action: { tag: "AddCrew", crew: allCrew.tank_03 }};
      },
      inputs: [],
      description: "add Tank03",
    },
  ],
  tag: "crew",
};

export const cardCrew_0007: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1003 },
  name: "Util01",
  effects: [
    { effect: (_obj) => {
      return { action: { tag: "AddCrew", crew: allCrew.util_01 }};
      },
      inputs: [],
      description: "add Util01",
    },
  ],
  tag: "crew",
};


export const cardEnemy_0001: Card = {
  origin: { tag: "PlayerOrigin", cardId: 2001 },
  name: "Boss01",
  effects: [
    { effect: (_obj) => {
      return { action: { tag: "AddEnemy", enemy: allEnemies.enemyBoss1 }};
      },
      inputs: [],
      description: "add Boss01",
    },
  ],
  tag: "enemy",
};

export const cardEnemy_0002: Card = {
  origin: { tag: "PlayerOrigin", cardId: 2001 },
  name: "Boss02",
  effects: [
    { effect: (_obj) => {
        return { action: {
          tag: "CombinedAction",
          actions: [
            { tag: "AddEnemy", enemy: allEnemies.enemyBoss2_1 },
            { tag: "AddEnemy", enemy: allEnemies.enemyBoss2_2 },
          ],
        }};
      },
      inputs: [],
      description: "add Boss02",
    },
  ],
  tag: "enemy",
};

/*
export const cardCrew_0003: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1003 },
  name: "ArmorSelf",
  effects: [
    { effect: (_obj) => {
      return { tag: "AddCrew", crew: allCrew.armorOnSelfHeal };
      },
      inputs: [],
      description: "add ArmorSelf",
    },
  ],
  tag: "crew",
};

export const cardCrew_0004: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1004 },
  name: "RegenDmg",
  effects: [
    { effect: (_obj) => {
      return { tag: "AddCrew", crew: allCrew.regenOnDamageAlly };
      },
      inputs: [],
      description: "add RegenDmg",
    },
  ],
  tag: "crew",
};

export const cardCrew_0005: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1005 },
  name: "Tank1",
  effects: [
    { effect: (_obj) => {
      return { tag: "AddCrew", crew: allCrew.tank1 };
      },
      inputs: [],
      description: "add Tank1",
    },
  ],
  tag: "crew",
};

export const cardCrew_0006: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1006 },
  name: "Dmg1",
  effects: [
    { effect: (_obj) => {
      return { tag: "AddCrew", crew: allCrew.dmg1 };
      },
      inputs: [],
      description: "add Dmg1",
    },
  ],
  tag: "crew",
};

export const cardCrew_0007: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1007 },
  name: "Dmg2",
  effects: [
    { effect: (obj) => {
      return allCrew.onAllAlly(obj.state, 
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
      },
      inputs: [],
      description: "doom 0/50 all allies",
    },
    { effect: (_obj) => {
      return { tag: "AddCrew", crew: allCrew.dmg1 };
      },
      inputs: [],
      description: "add Dmg1",
    },
  ],
  tag: "crew",
};

export const cardCrew_0008: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1008 },
  name: "DmgPoison",
  effects: [
    { effect: (_obj) => {
      return { tag: "AddCrew", crew: allCrew.dmgPoison };
      },
      inputs: [],
      description: "add DmgPoison",
    },
  ],
  tag: "crew",
};

export const cardCrew_0009: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1009 },
  name: "BasicCrew1",
  effects: [
    { effect: (_obj) => {
      return { tag: "AddCrew", crew: allCrew.basicCrew1 };
      },
      inputs: [],
      description: "add BasicCrew1",
    },
  ],
  tag: "crew",
};

export const cardCrew_0012: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1012 },
  name: "Tank01",
  effects: [
    { effect: (_obj) => {
      return { tag: "AddCrew", crew: allCrew.tank_01 };
      },
      inputs: [],
      description: "add Tank01",
    },
  ],
  tag: "crew",
};

export const cardCrew_0013: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1013 },
  name: "Dmg01",
  effects: [
    { effect: (_obj) => {
      return { tag: "AddCrew", crew: allCrew.dmg_01 };
      },
      inputs: [],
      description: "add Dmg01",
    },
  ],
  tag: "crew",
};

export const cardCrew_0014: Card = {
  origin: { tag: "PlayerOrigin", cardId: 1014 },
  name: "Util01",
  effects: [
    { effect: (_obj) => {
      return { tag: "AddCrew", crew: allCrew.util_01 };
      },
      inputs: [],
      description: "add Util01",
    },
  ],
  tag: "crew",
};

export const cardItem_0000: Card = {
  origin: { tag: "PlayerOrigin", cardId: 2000 },
  name: "Shield",
  effects: [
    { effect: (_obj) => {
      return { tag: "PayGold", pay: 5 };
      },
      inputs: [],
      description: "pay 5",
    },
    { effect: (_obj) => {
      return { tag: "AddItem", item: allItems.guard1StartCombat };
      },
      inputs: [],
      description: "add item",
    },
  ],
  tag: "item",
};

export const cardBattle_0009: Card = {
  origin: { tag: "PlayerOrigin", cardId: 3009 },
  name: "Enemy9",
  effects: [
    { effect: (_obj) => {
      return { tag: "AddEnemy", enemy: allEnemies.enemyBoss1 };
      },
      inputs: [],
      description: "add Enemy9",
    },
  ],
  tag: "enemy",
};

export const cardBattle_0010: Card = {
  origin: { tag: "PlayerOrigin", cardId: 3010 },
  name: "Enemy10",
  effects: [
    { effect: (_obj) => {
      return { tag: "AddEnemy", enemy: allEnemies.enemyBoss2 };
      },
      inputs: [],
      description: "add Enemy10",
    },
  ],
  tag: "enemy",
};

export const cardBattle_0011: Card = {
  origin: { tag: "PlayerOrigin", cardId: 3011 },
  name: "Dummy",
  effects: [
    { effect: (_obj) => {
      return { tag: "AddEnemy", enemy: allEnemies.dummy };
      },
      inputs: [],
      description: "add Dummy",
    },
  ],
  tag: "enemy",
};

export const cardBattle_0012: Card = {
  origin: { tag: "PlayerOrigin", cardId: 3012 },
  name: "BasicEnemy1",
  effects: [
    { effect: (_obj) => {
      return { tag: "AddEnemy", enemy: allEnemies.basicEnemy1 };
      },
      inputs: [],
      description: "add BasicEnemy1",
    },
  ],
  tag: "enemy",
};

export const cardBattle_0013: Card = {
  origin: { tag: "PlayerOrigin", cardId: 3013 },
  name: "BasicEnemy2",
  effects: [
    { effect: (_obj) => {
      return { tag: "AddEnemy", enemy: allEnemies.basicEnemy2 };
      },
      inputs: [],
      description: "add BasicEnemy2",
    },
  ],
  tag: "enemy",
};

export const cardBattle_0014: Card = {
  origin: { tag: "PlayerOrigin", cardId: 3014 },
  name: "Boss3",
  effects: [
    { effect: (_obj) => {
      return { tag: "AddEnemy", enemy: allEnemies.enemyBoss3 };
      },
      inputs: [],
      description: "add Boss3",
    },
  ],
  tag: "enemy",
};
*/