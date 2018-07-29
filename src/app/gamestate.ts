import { focus, over, set } from "src/shared/iassign-util";
import { Solution } from "src/shared/game/solution";
import { Card, Rest, Event } from "src/shared/game/card";

export type Limit = {
  limit: number,
};

export type LimitedCard = Card & Limit;

export type LeftMenuOption = "crew" | "enemy" | "item" | "general" | "rest";

export const allLeftMenuOptions: LeftMenuOption[] = ["crew", "enemy", "item", "general", "rest"];

export type GameState = {
  solution: Solution,
  availableCards: LimitedCard[],
  selectedLeftMenu: LeftMenuOption,
};

type ParamCallBack<A> = (a: A) => void;

const solutionCallbacks: ParamCallBack<Solution>[] = [];
const cardsCallbacks: ParamCallBack<LimitedCard[]>[] = [];
const leftMenuCallbacks: ParamCallBack<LeftMenuOption>[] = [];

const initialGameState: GameState = {
  solution: { paths: [] },
  availableCards: [],
  selectedLeftMenu: "crew",
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

export function minusLimit(limitTexts: Phaser.Text[], index: number) {
  if (gameState.solution.paths.length == 0) {
    return "noPaths";
  } else if (gameState.availableCards[index].limit <= 0) {
    return "limitIsZero";
  } else {
    gameState = focus(gameState,
      over(x => x.availableCards[index].limit, x => x - 1)
    );
    limitTexts[index].setText(gameState.availableCards[index].limit.toString());
    return "cardUsed";
  }
}

export function plusLimit(limitTexts: Phaser.Text[], cardId: number) {
  const index = findIdAvailableCard(cardId);
  if (index === "notFound") {
    throw ("index " + index + " not found in available cards");
  }
  gameState = focus(gameState,
    over(x => x.availableCards[index].limit, x => x + 1)
  );
  limitTexts[index].setText(gameState.availableCards[index].limit.toString());
}

function findIdAvailableCard(cardId: number) {
  let index: number | "notFound" = "notFound";
  let i = 0;
  for (const card of gameState.availableCards) {
    if (card.id === cardId) {
      index = i;
    }
    i += 1;
  }
  return index;
}

export function addToSolution(event: Event) {
  const solution = focus(gameState.solution,
    over(x => x.paths[x.paths.length - 1].eventCards, x => x.concat(event)),
  );
  changeSolution(solution);
}

export function addRestToSolution(rest: Rest) {
  const solution = focus(gameState.solution,
    over(x => x.paths, x => x.concat({ restCard: rest, eventCards: [] })),
  );
  changeSolution(solution);
}

export function removeCardFromSolution(pathIndex: number, cardIndex: number) {
  const solution = focus(gameState.solution,
    over(x => x.paths[pathIndex].eventCards, x => x.slice(0, cardIndex).concat(x.slice(cardIndex + 1, x.length))),
  );
  changeSolution(solution);
}

export function removePathFromSolution(limitTexts: Phaser.Text[], pathIndex: number) {
  return function() {
    for (const card of gameState.solution.paths[pathIndex].eventCards) {
      plusLimit(limitTexts, card.id);
    }
    const solution = focus(gameState.solution,
      over (x => x.paths, x => x.slice(0, pathIndex).concat(x.slice(pathIndex + 1, x.length))),
    );
    changeSolution(solution);
  };
}

export function addSolutionCallback(cb: ParamCallBack<Solution>) {
  solutionCallbacks.push(cb);
}

export function changeAvailableCards(cards: LimitedCard[]) {
  gameState = focus(gameState, set(x => x.availableCards, cards));
  cardsCallbacks.forEach(cb => cb(cards));
}

export function addCardsCallback(cb: ParamCallBack<LimitedCard[]>) {
  cardsCallbacks.push(cb);
}

export function addLeftMenuCallback(cb: ParamCallBack<LeftMenuOption>) {
  leftMenuCallbacks.push(cb);
}

export function changeLeftMenu(leftMenuOption: LeftMenuOption) {
  gameState = focus(gameState, over(x => x.selectedLeftMenu, x => leftMenuOption));
  leftMenuCallbacks.forEach(cb => cb(leftMenuOption));
}