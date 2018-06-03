import { generateEffects, negEffects } from "src/shared/rules/generateEffect";
import { newRng, rngHandler } from "src/shared/handler/rng/randomSeedRng";

function test1() {
  const effects = generateEffects(10, rngHandler(newRng("ABCD-EFGH")), negEffects);
  console.log(JSON.stringify(effects));
}

test1();