import expect from "expect";
import { Solution, Path, runSolution } from "src/shared/game/solution";
import { allCrew } from "src/shared/game/crew";
import { allItems } from "src/shared/game/item";
import { showSolutionLog } from "src/shared/game/log";

function basicCrewTest() {
  const path1: Path = {
    restCard: { actions: [{ tag: "Rest" }], id: 0, tag: "rest", subtag: "general" },
    eventCards: [
      { actions: [{ tag: "AddCrew", crew: allCrew.recruitGrow1 }], id: 0, tag: "event", subtag: "general" },
      { actions: [{ tag: "AddCrew", crew: allCrew.stFighter }], id: 0, tag: "event", subtag: "general" },
      { actions: [{ tag: "AddCrew", crew: allCrew.stFighter }], id: 0, tag: "event", subtag: "general" },
    ]
  }
  const solution: Solution = {
    paths: [path1]
  }

  const sol = runSolution(solution);
  if (sol.state === "invalid") {
    console.log(showSolutionLog(sol.log));
    console.log("invalid");
  } else {
    const { state, log } = sol;
    console.log(JSON.stringify(state));
    console.log(showSolutionLog(log));
    expect(state.crew[0].hp).toEqual(3);
    expect(state.crew[0].ap).toEqual(3);
  }
}


function basicBattleTest() {
  const path1: Path = {
    restCard: { actions: [{ tag: "Rest" }], id: 0, tag: "rest", subtag: "general" },
    eventCards: [
      { actions: [{ tag: "AddCrew", crew: allCrew.stFighter }], id: 0, tag: "event", subtag: "general" },
      { actions: [{ tag: "AddCrew", crew: allCrew.stRanged }], id: 0, tag: "event", subtag: "general" },
      { actions: [{ tag: "AddEnemy", enemy: { hp: 10, maxHp: 10, actions: [
        {
          tag: "AddStatus",
          target: { tag: "Self" },
          status: {
            tag: "Regen",
            value: 2,
          },
          next: { tag: "NextId" },
        },
        {
          tag: "Damage",
          target: { tag: "Positions", type: "ally", positions: [0] },
          value: 2,
          next: { tag: "NextId" },
        },
        ], triggers: [
          {
            onTag: "Death",
            type: "before",
            action: {
              tag: "GainGold",
              gain: 5,
            },
            conditions: [
              { tag: "OwnId" },
            ],
          }
        ]
      } 
    },
      ], id: 0, tag: "event", subtag: "general" },
      { actions: [{ tag: "BattleTurn" }], id: 0, tag: "event", subtag: "general" },
      { actions: [{ tag: "BattleTurn" }], id: 0, tag: "event", subtag: "general" },
      { actions: [{
        tag: "PayGold",
        pay: 5,
      },
      {
        tag: "AddItem",
        item: { triggers: [] },
      }
      ], id: 0, tag: "event", subtag: "general" },
    ]
  }
  const solution: Solution = {
    paths: [path1]
  }

  const sol = runSolution(solution);
  if (sol.state === "invalid") {
    console.log(showSolutionLog(sol.log));
    console.log("invalid");
  } else {
    const { state, log } = sol;
    console.log(JSON.stringify(state));
    console.log(showSolutionLog(log));
  }
}

function basicStatusTest() {
  const path1: Path = {
    restCard: { actions: [{ tag: "Rest" }], id: 0, tag: "rest", subtag: "general" },
    eventCards: [
      { actions: [
        { tag: "AddCrew", crew: allCrew.stFighter },
        { tag: "AddStatus", status: { tag: "Poison" , value: 3 }, target: { tag: "Target", type: "ally", positions: [0] } },
      ], id: 0, tag: "event", subtag: "general" },
      { actions: [
        { tag: "Rest" },
      ], id: 0, tag: "event", subtag: "general" },
      { actions: [
        { tag: "Rest" },
      ], id: 0, tag: "event", subtag: "general" },
      { actions: [
        { tag: "Rest" },
      ], id: 0, tag: "event", subtag: "general" },
      { actions: [
        { tag: "Rest" },
      ], id: 0, tag: "event", subtag: "general" },
      { actions: [
        { tag: "Rest" },
      ], id: 0, tag: "event", subtag: "general" },
    ]
  }
  const solution: Solution = {
    paths: [path1]
  }

  const sol = runSolution(solution);
  if (sol.state === "invalid") {
    console.log(showSolutionLog(sol.log));
    console.log("invalid");
  } else {
    const { state, log } = sol;
    console.log(JSON.stringify(state));
    console.log(showSolutionLog(log));
  }
}

function basicApDamageTest() {
  const path1: Path = {
    restCard: { actions: [{ tag: "Rest" }], id: 0, tag: "rest", subtag: "general" },
    eventCards: [
      { actions: [{ tag: "AddCrew", crew: {
        ap: 0,
        hp: 5,
        maxHp: 5,
        triggers: [],
        ranged: false,
        actions: [{
          tag: "ApDamage",
          target: { tag: "Positions", type: "enemy", positions: [0] },
          multiplier: 1,
        }],
        abilities: [],
      }, }
      ]
      , id: 0, tag: "event", subtag: "general" },
      { actions: [{ tag: "AddEnemy", enemy: { hp: 2, maxHp: 2, actions: [
        {
          tag: "Damage",
          target: { tag: "Positions", type: "ally", positions: [0] },
          value: 1,
          next: { tag: "NextId" },
        },
        ], triggers: [] } },
        { tag: "GainGold", gain: 5 },
      ], id: 0, tag: "event", subtag: "general" },
      { actions: [{ tag: "BattleTurn" }], id: 0, tag: "event", subtag: "general" },
    ]
  }
  const solution: Solution = {
    paths: [path1]
  }

  const sol = runSolution(solution);
  if (sol.state === "invalid") {
    console.log(showSolutionLog(sol.log));
    console.log("invalid");
  } else {
    const { state, log } = sol;
    console.log(JSON.stringify(state));
    console.log(showSolutionLog(log));
  }
}

function retaliateTest() {
  const path1: Path = {
    restCard: { actions: [{ tag: "Rest" }], id: 0, tag: "rest", subtag: "general" },
    eventCards: [
      { actions: [{ tag: "AddCrew", crew: allCrew.stFighter }], id: 0, tag: "event", subtag: "general" },
      { actions: [{ tag: "AddCrew", crew: allCrew.stRanged }], id: 0, tag: "event", subtag: "general" },
      { actions: [{ tag: "AddEnemy", enemy: { hp: 100, maxHp: 100, actions: [
        {
          tag: "Damage",
          target: { tag: "Positions", type: "ally", positions: [] },
          value: 0,
          next: { tag: "NextId" },
        },
        ], triggers: 
          [
            {
              onTag: "Damage",
              type: "before",
              action: {
                tag: "Damage",
                target: { tag: "OriginTarget" },
                value: 1,
              },
              conditions: [
                { tag: "TypeCondition", type: "enemy" },
              ],
            },
          ]
        } 
      },
      ], id: 0, tag: "event", subtag: "general" },
      { actions: [{ tag: "BattleTurn" }], id: 0, tag: "event", subtag: "general" },
      { actions: [{ tag: "BattleTurn" }], id: 0, tag: "event", subtag: "general" },
    ]
  }
  const solution: Solution = {
    paths: [path1]
  }

  const sol = runSolution(solution);
  if (sol.state === "invalid") {
    console.log(showSolutionLog(sol.log));
    console.log("invalid");
  } else {
    const { state, log } = sol;
    console.log(JSON.stringify(state));
    console.log(showSolutionLog(log));
  }
}


// basicCrewTest();
// basicBattleTest();
// basicStatusTest();
// basicItemTest();
// guard3ItemTest();
// basicApDamageTest();
retaliateTest();

/*

function basicDeathTest() {
  const path1: Path = {
    restCard: { actions: [{ tag: "Rest" }], id: 0, tag: "rest" },
    eventCards: [
      { actions: [{ tag: "Recruit", crew: allCrew.stFighter }], id: 0, tag: "event" },
      { actions: [{ tag: "Recruit", crew: allCrew.stFighter }], id: 0, tag: "event" },
      { actions: [{ tag: "Battle", enemy: { rank: 8, actions: [{
        tag: "MeleeAttack",
        multiplier: 1,
        positions: [0], }] } },
        { tag: "GainGold", gain: 5 },
      ], id: 0, tag: "event" },
    ]
  }
  const solution: Solution = {
    paths: [path1]
  }

  const sol = runSolution(solution);
  if (sol.state === "invalid") {
    console.log(showSolutionLog(sol.log));
    console.log("invalid");
  } else {
    const { state, log } = sol;
    console.log(JSON.stringify(state));
    console.log(showSolutionLog(log));
  }
}

function basicItemTest() {
  const path1: Path = {
    restCard: { actions: [{ tag: "Rest" }], id: 0, tag: "rest" },
    eventCards: [
      { actions: [{ tag: "Recruit", crew: allCrew.recruitGainAPWhenHP }], id: 0, tag: "event" },
      { actions: [{ tag: "AddItem", item: allItems.plus11StartCombat }], id: 0, tag: "event" },
      { actions: [{ tag: "Battle", enemy: { rank: 6, actions: [{
        tag: "MeleeAttack",
        multiplier: 1,
        positions: [0], }] } },
        { tag: "GainGold", gain: 5 },
      ], id: 0, tag: "event" },
    ]
  }
  const solution: Solution = {
    paths: [path1]
  }

  const sol = runSolution(solution);
  if (sol.state === "invalid") {
    console.log(showSolutionLog(sol.log));
    console.log("invalid");
  } else {
    const { state, log } = sol;
    console.log(JSON.stringify(state));
    console.log(showSolutionLog(log));
  }
}

function guard3ItemTest() {
  const path1: Path = {
    restCard: { actions: [{ tag: "Rest" }], id: 0, tag: "rest" },
    eventCards: [
      { actions: [{ tag: "Recruit", crew: allCrew.stFighter }], id: 0, tag: "event" },
      { actions: [{ tag: "AddItem", item: allItems.guard3StartCombat }], id: 0, tag: "event" },
      { actions: [{ tag: "Battle", enemy: { rank: 0, actions: [{
        tag: "MeleeAttack",
        multiplier: 1,
        positions: [0], }] } },
        { tag: "GainGold", gain: 5 },
      ], id: 0, tag: "event" },
    ]
  }
  const solution: Solution = {
    paths: [path1]
  }

  const sol = runSolution(solution);
  if (sol.state === "invalid") {
    console.log(showSolutionLog(sol.log));
    console.log("invalid");
  } else {
    const { state, log } = sol;
    console.log(JSON.stringify(state));
    console.log(showSolutionLog(log));
  }
}
*/