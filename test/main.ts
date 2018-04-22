import { Board, generateBoard, boardData } from "src/shared/board";
import { rng } from "src/shared/handler/rng/randomSeedRng";

console.log(generateBoard(rng, boardData));