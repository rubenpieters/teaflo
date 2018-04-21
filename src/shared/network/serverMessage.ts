import { Board } from "src/shared/board";

type CurrentBoard = {
  tag: "CurrentBoard",
  board: Board,
}

export type ServerMessage = CurrentBoard;