import { Solution, Path, runSolution } from "src/shared/game/solution";
import { allCrew } from "src/shared/game/crew";
import { showSolutionLog } from "src/shared/game/log";

function basicCrewTest() {
  const path1: Path = {
    restAction: { tag: "Rest" },
    actions: [{ tag: "Recruit", crew: allCrew.stFighter }]
  }
  const solution: Solution = {
    paths: [path1]
  }

  const sol = runSolution(solution);
  if (sol === "invalid") {
    console.log("invalid");
  } else {
    const { state, log } = sol;
    console.log(JSON.stringify(state));
    console.log(showSolutionLog(log));
  }
}

function basicBattleTest() {
  const path1: Path = {
    restAction: { tag: "Rest" },
    actions: [
      { tag: "Recruit", crew: allCrew.stFighter },
      { tag: "Battle", enemy: { rank: 2, actions: [{
        tag: "MeleeAttack",
        multiplier: 1,
        positions: [0], }] } },
    ]
  }
  const solution: Solution = {
    paths: [path1]
  }

  const sol = runSolution(solution);
  if (sol === "invalid") {
    console.log("invalid");
  } else {
    const { state, log } = sol;
    console.log(JSON.stringify(state));
    console.log(showSolutionLog(log));
  }
}

//basicCrewTest();
basicBattleTest();