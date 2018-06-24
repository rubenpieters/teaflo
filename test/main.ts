import { Board, generateBoard, boardData } from "src/shared/board";
import { newRng, rngHandler } from "src/shared/handler/rng/randomSeedRng";
import iassign from "immutable-assign";

console.log(generateBoard(rngHandler(newRng("seed")), boardData));
