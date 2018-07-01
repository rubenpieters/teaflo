import { Solution, Path, runSolution } from "src/shared/game/solution";
import { allCrew } from "src/shared/game/crew";

function basicCrewTest() {
  const path1: Path = {
    restAction: { tag: "Rest" },
    actions: [{ tag: "Recruit", crew: allCrew.stFighter }]
  }
  const solution: Solution = {
    paths: [path1]
  }

  console.log(JSON.stringify(runSolution(solution)));
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

  console.log(JSON.stringify(runSolution(solution)));
}

//basicCrewTest();
basicBattleTest();