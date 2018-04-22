"use strict";
exports.__esModule = true;
var board_1 = require("src/shared/board");
var randomSeedRng_1 = require("src/shared/handler/rng/randomSeedRng");
console.log(board_1.generateBoard(randomSeedRng_1.rng, board_1.boardData));
