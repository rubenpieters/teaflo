import { Card } from "src/shared/game/solution";
import { allCrew } from "src/shared/game/crew";
import { allItems } from "src/shared/game/item";
import { allEnemies } from "src/shared/game/enemy";

const cardCrew_0000: Card = {
  actions: [{ tag: "Recruit", crew: allCrew.stFighter }],
  id: 0,
};
const cardCrew_0001: Card = {
  actions: [{ tag: "Recruit", crew: allCrew.stRanged }],
  id: 1,
};

const cardItem_0000: Card = {
  actions: [
    { tag: "PayGold", pay: 5 },
    { tag: "AddItem", item: allItems.guard3StartCombat },
  ],
  id: 2,
};

const cardBattle_0000: Card = {
  actions: [
    { tag: "Battle", enemy: allEnemies.enemyAtk012 },
    { tag: "GainGold", gain: 10 },
  ],
  id: 3,
};

const cardBattle_0001: Card = {
  actions: [
    { tag: "Battle", enemy: allEnemies.enemyHeal2R14 },
    { tag: "GainGold", gain: 7 },
  ],
  id: 4,
};

export const allCards = {
  cardCrew_0000: cardCrew_0000,
  cardCrew_0001: cardCrew_0001,

  cardItem_0000: cardItem_0000,

  cardBattle_0000: cardBattle_0000,
  cardBattle_0001: cardBattle_0001,
};