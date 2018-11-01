import { changeConnected } from "src/app/appstate";
import { Board, chAvailableCards, LimitedCard, resetSolution } from "src/app/gamestate";
import * as allCards from "src/shared/data/card";
import { ServerMessage } from "src/shared/network/serverMessage";

export type ServerConnection = {
  socket: WebSocket,
};

export function connectToServer(cb: (serverConn: ServerConnection) => void): ServerConnection {
  const host: string = location.origin.replace(/^http/, "ws").replace(/localhost:3000/, "localhost:8080");
  const socket = new WebSocket(host);
  const serverConn = { socket: socket };
  socket.onopen = () => {
    changeConnected("connected");
    socket.onmessage = onServerMessage;

    cb(serverConn);
  };
  return { socket: socket };
}

export function getBoard(_serverConn: ServerConnection, board: Board, seed: string) {
  /*const getBoardMessage: ClientMessage = {
    tag: "GetCurrentBoard",
    seed: seed,
  };

  serverConn.socket.send(JSON.stringify(getBoardMessage));*/
  let cards: LimitedCard[];
  if (seed === "1") {
    cards = [
      {...allCards.cardBattleTurn, limit: Infinity },
      {...allCards.cardCrew_0009, limit: Infinity },
      {...allCards.cardEnemy_0002, limit: Infinity },
    ];
  } else if (seed === "2") {
    cards = [
      {...allCards.cardBattleTurn, limit: Infinity },
      {...allCards.cardCrew_0009, limit: Infinity },
      {...allCards.cardCrew_0010, limit: Infinity },
      {...allCards.cardCrew_0011, limit: Infinity },
      {...allCards.cardEnemy_0003, limit: Infinity },
    ];
  } else if (seed === "3") {
    cards = [
      {...allCards.cardBattleTurn, limit: Infinity },
    ];
  } else if (seed === "4") {
    cards = [
      {...allCards.cardBattleTurn, limit: Infinity },
    ];
  } else if (seed === "5") {
    cards = [
      {...allCards.cardBattleTurn, limit: Infinity },
    ];
  } else if (seed === "sandbox") {
    cards = [
      {...allCards.cardBattleTurn, limit: Infinity },
      {...allCards.cardDummy, limit: Infinity },
      {...allCards.cardDummyDmg1, limit: Infinity },
      {...allCards.cardCrew_0001, limit: Infinity },
      {...allCards.cardCrew_0002, limit: Infinity },
      {...allCards.cardCrew_0003, limit: Infinity },
      {...allCards.cardCrew_0004, limit: Infinity },
      {...allCards.cardCrew_0005, limit: Infinity },
      {...allCards.cardCrew_0006, limit: Infinity },
      {...allCards.cardCrew_0007, limit: Infinity },
      {...allCards.cardCrew_0008, limit: Infinity },
      {...allCards.cardEnemy_0001, limit: Infinity },
      {...allCards.cardEnemy_0002, limit: Infinity },
    ];
  } else {
    throw "unexpected seed: " + seed;
  }
  chAvailableCards(board, cards);
  resetSolution(board);
}

function onServerMessage(event: MessageEvent): void {
  const serverMsg: ServerMessage = JSON.parse(event.data);
  switch (serverMsg.tag) {
    case "CurrentBoard": {
      return;
    }
  }
}