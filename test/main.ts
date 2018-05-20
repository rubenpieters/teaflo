import { Board, generateBoard, boardData } from "src/shared/board";
import { newRng, rngHandler } from "src/shared/handler/rng/randomSeedRng";

console.log(generateBoard(rngHandler(newRng("seed")), boardData));
