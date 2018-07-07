import { Solution } from "src/shared/game/solution";

export type GameState = {
  solution: Solution,
};

type ParamCallBack<A> = (a: A) => void;

const solutionCallbacks: ParamCallBack<Solution>[] = [];

const gameState: GameState = {
  solution: { paths: [] },
};

export function changeSolution(solution: Solution) {
  gameState.solution = solution;
  solutionCallbacks.forEach(cb => cb(solution));
}

export function addSolutionCallback(cb: ParamCallBack<Solution>) {
  solutionCallbacks.push(cb);
}
