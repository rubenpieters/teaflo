import { mkGameState, showGamestate } from "../../src/shared/game/state";
import { extendSolution, emptySolution, runSolution } from "../../src/shared/game/solution";
import { trinity_sup_ab1, trinity_tnk_ab1, trinity_dmg_ab1 } from "../../src/shared/data/act2/frUnits";
import { friendlyId, enemyId } from "../../src/shared/definitions/entityId";

export function run(
): string {
  let result: string = "";

  const initState = mkGameState(
    ["trinity_dmg", "trinity_tnk", "trinity_sup"],
    ["a2l1_en"],
  );
  
  result += "-------------------------------------------\n";
  result += showGamestate(initState);
  
  const _sol0 = extendSolution({
    ability: trinity_tnk_ab1,
    origin: friendlyId(1),
    inputs: [],
  }, emptySolution, []);
  
  const sol0 = runSolution(_sol0.solution, _sol0.loc, initState);
  
  result += "-------------------------------------------\n";
  result += showGamestate(sol0.state);
  
  const _sol1 = extendSolution({
    ability: trinity_sup_ab1,
    origin: friendlyId(2),
    inputs: [friendlyId(1)],
  }, emptySolution, []);
  
  const sol1 = runSolution(_sol1.solution, _sol1.loc, sol0.state);
  
  result += "-------------------------------------------\n";
  result += showGamestate(sol1.state);
  
  const _sol2 = extendSolution({
    ability: trinity_dmg_ab1,
    origin: friendlyId(0),
    inputs: [enemyId(3)],
  }, emptySolution, []);
  
  const sol2 = runSolution(_sol2.solution, _sol2.loc, sol1.state);
  
  result += "-------------------------------------------\n";
  result += showGamestate(sol2.state);
  
  const _sol3 = extendSolution({
    ability: trinity_sup_ab1,
    origin: friendlyId(2),
    inputs: [friendlyId(1)],
  }, emptySolution, []);
  
  const sol3 = runSolution(_sol3.solution, _sol3.loc, sol2.state);
  
  result += "-------------------------------------------\n";
  result += showGamestate(sol3.state);
  
  const _sol4 = extendSolution({
    ability: trinity_dmg_ab1,
    origin: friendlyId(0),
    inputs: [enemyId(3)],
  }, emptySolution, []);
  
  const sol4 = runSolution(_sol4.solution, _sol4.loc, sol3.state);
  
  result += "-------------------------------------------\n";
  result += showGamestate(sol4.state);
  
  const _sol5 = extendSolution({
    ability: trinity_dmg_ab1,
    origin: friendlyId(0),
    inputs: [enemyId(3)],
  }, emptySolution, []);
  
  const sol5 = runSolution(_sol5.solution, _sol5.loc, sol4.state);
  
  result += "-------------------------------------------\n";
  result += showGamestate(sol5.state);

  return result;
}

//console.log(run());