import { focus, over, set } from "src/shared/iassign-util";
import { Solution, Card } from "src/shared/game/solution";
import { Rest } from "src/shared/game/action";

export type GameState = {
  solution: Solution,
  availableCards: Card[],
};

type ParamCallBack<A> = (a: A) => void;

const solutionCallbacks: ParamCallBack<Solution>[] = [];
const cardsCallbacks: ParamCallBack<Card[]>[] = [];

const initialGameState: GameState = {
  solution: { paths: [] },
  availableCards: [],
};

let gameState: GameState = initialGameState;

export function resetGameState() {
  changeAvailableCards(initialGameState.availableCards);
  changeSolution(initialGameState.solution);
}

export function changeSolution(solution: Solution) {
  gameState = focus(gameState, set(x => x.solution, solution));
  solutionCallbacks.forEach(cb => cb(solution));
}

export function addToSolution(card: Card) {
  const solution = focus(gameState.solution,
    over (x => x.paths[x.paths.length - 1].cards, x => x.concat([card])),
  );
  changeSolution(solution);
}

export function addRestToSolution(rest: Rest) {
  const solution = focus(gameState.solution,
    over (x => x.paths, x => x.concat({ restAction: rest, cards: [] })),
  );
  changeSolution(solution);
}

export function removeCardFromSolution(pathIndex: number, cardIndex: number) {
  const solution = focus(gameState.solution,
    over (x => x.paths[pathIndex].cards, x => x.slice(0, cardIndex).concat(x.slice(cardIndex + 1, x.length))),
  );
  changeSolution(solution);
}

export function removePathFromSolution(pathIndex: number) {
  return function() {
    const solution = focus(gameState.solution,
      over (x => x.paths, x => x.slice(0, pathIndex).concat(x.slice(pathIndex + 1, x.length))),
    );
    changeSolution(solution);
  };
}

export function addSolutionCallback(cb: ParamCallBack<Solution>) {
  solutionCallbacks.push(cb);
}

export function changeAvailableCards(cards: Card[]) {
  console.log("CARDS: " + cards.length);
  gameState = focus(gameState, set(x => x.availableCards, cards));
  cardsCallbacks.forEach(cb => cb(cards));
}

export function addCardsCallback(cb: ParamCallBack<Card[]>) {
  cardsCallbacks.push(cb);
}
