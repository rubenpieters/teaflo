import expect from "expect";
import { Solution, Path, runSolution } from "src/shared/game/solution";
import { allCrew } from "src/shared/game/crew";
import { allItems } from "src/shared/game/item";
import { showSolutionLog } from "src/shared/game/log";

function basicCrewTest() {
  const path1: Path = {
    restAction: { tag: "Rest" },
    cards: [
      { actions: [{ tag: "Recruit", crew: allCrew.recruitGrow1 }], id: 0 },
      { actions: [{ tag: "Recruit", crew: allCrew.stFighter }], id: 0 },
      { actions: [{ tag: "Recruit", crew: allCrew.stFighter }], id: 0 },
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
    restAction: { tag: "Rest" },
    cards: [
      { actions: [{ tag: "Recruit", crew: allCrew.stFighter }], id: 0 },
      { actions: [{ tag: "Recruit", crew: allCrew.stRanged }], id: 0 },
      { actions: [{ tag: "Battle", enemy: { rank: 2, actions: [{
        tag: "MeleeAttack",
        multiplier: 1,
        positions: [0], }] } },
        { tag: "GainGold", gain: 5 },
      ], id: 0 },
      { actions: [{
        tag: "PayGold",
        pay: 5,
      },
      {
        tag: "AddItem",
        item: { triggers: [] },
      }
      ], id: 0 },
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

function basicDeathTest() {
  const path1: Path = {
    restAction: { tag: "Rest" },
    cards: [
      { actions: [{ tag: "Recruit", crew: allCrew.stFighter }], id: 0 },
      { actions: [{ tag: "Recruit", crew: allCrew.stFighter }], id: 0 },
      { actions: [{ tag: "Battle", enemy: { rank: 8, actions: [{
        tag: "MeleeAttack",
        multiplier: 1,
        positions: [0], }] } },
        { tag: "GainGold", gain: 5 },
      ], id: 0 },
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
    restAction: { tag: "Rest" },
    cards: [
      { actions: [{ tag: "Recruit", crew: allCrew.recruitGainAPWhenHP }], id: 0 },
      { actions: [{ tag: "AddItem", item: allItems.plus11StartCombat }], id: 0 },
      { actions: [{ tag: "Battle", enemy: { rank: 6, actions: [{
        tag: "MeleeAttack",
        multiplier: 1,
        positions: [0], }] } },
        { tag: "GainGold", gain: 5 },
      ], id: 0 },
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
    restAction: { tag: "Rest" },
    cards: [
      { actions: [{ tag: "Recruit", crew: allCrew.stFighter }], id: 0 },
      { actions: [{ tag: "AddItem", item: allItems.guard3StartCombat }], id: 0 },
      { actions: [{ tag: "Battle", enemy: { rank: 0, actions: [{
        tag: "MeleeAttack",
        multiplier: 1,
        positions: [0], }] } },
        { tag: "GainGold", gain: 5 },
      ], id: 0 },
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
basicBattleTest();
// basicItemTest();
// guard3ItemTest();