import { Action } from "src/shared/game/action";
import { Target } from "src/shared/game/target";
import { allCrew } from "src/shared/game/crew";
import { allItems } from "src/shared/game/item";
import { allEnemies } from "src/shared/game/enemy";

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

const cardCrew_0000: Card = {
  id: 0,
  actions: [{ tag: "AddCrew", crew: allCrew.stFighter }],
  tag: "event",
};
const cardCrew_0001: Card = {
  id: 1,
  actions: [{ tag: "AddCrew", crew: allCrew.stRanged }],
  tag: "event",
};

const cardItem_0000: Card = {
  id: 2,
  actions: [
    { tag: "PayGold", pay: 5 },
    { tag: "AddItem", item: allItems.guard3StartCombat },
  ],
  tag: "event",
};

const cardBattle_0000: Card = {
  id: 3,
  actions: [
    { tag: "AddEnemy", enemy: allEnemies.enemyAtk012 },
    { tag: "GainGold", gain: 10 },
  ],
  tag: "event",
};

const cardBattle_0001: Card = {
  id: 4,
  actions: [
    // TODO: { tag: "Battle", enemy: allEnemies.enemyHeal2R14 },
    { tag: "GainGold", gain: 7 },
  ],
  tag: "event",
};

export const allCards = {
  cardCrew_0000: cardCrew_0000,
  cardCrew_0001: cardCrew_0001,

  cardItem_0000: cardItem_0000,

  cardBattle_0000: cardBattle_0000,
  cardBattle_0001: cardBattle_0001,
};