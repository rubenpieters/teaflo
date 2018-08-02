import { changeConnected } from "src/app/appstate";
import { Board, chAvailableCards, LimitedCard } from "src/app/gamestate";
import { Card, allCards } from "src/shared/game/card";
import { ServerMessage } from "src/shared/network/serverMessage";
import { ClientMessage } from "src/shared/network/clientMessage";

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

export function getBoard(serverConn: ServerConnection, board: Board, seed: string) {
  /*const getBoardMessage: ClientMessage = {
    tag: "GetCurrentBoard",
    seed: seed,
  };

  serverConn.socket.send(JSON.stringify(getBoardMessage));*/
  let cards: LimitedCard[];
  if (seed === "1") {
    cards = [
      {...allCards.cardBattleTurn, ...{ limit: Infinity }},
      {...allCards.cardCrew_0000, ...{ limit: 5 } },
      {...allCards.cardCrew_0001, ...{ limit: 5 } },
      {...allCards.cardItem_0000, ...{ limit: 5 } },
      {...allCards.cardBattle_0000, ...{ limit: 5 } },
      {...allCards.cardBattle_0001, ...{ limit: 5 } },
      {...allCards.cardBattle_0002, ...{ limit: 1 } },
    ];
  } else if (seed === "2") {
    cards = [
      {...allCards.cardBattleTurn, ...{ limit: Infinity }},
      {...allCards.cardRest, ...{ limit: Infinity }},
      {...allCards.cardCrew_0000, ...{ limit: 5 } },
      {...allCards.cardCrew_0001, ...{ limit: 5 } },
      {...allCards.cardBattle_0003, ...{ limit: 1 } },
      {...allCards.cardBattle_0004, ...{ limit: 1 } },
      {...allCards.cardBattle_0005, ...{ limit: 1 } },
      {...allCards.cardBattle_0006, ...{ limit: 1 } },
      {...allCards.cardBattle_0007, ...{ limit: 1 } },
      {...allCards.cardItem_0000, ...{ limit: 1 } },
    ];
  } else if (seed === "3") {
    cards = [
      {...allCards.cardBattleTurn, ...{ limit: Infinity }},
      {...allCards.cardRest, ...{ limit: Infinity }},
      {...allCards.cardCrew_0000, ...{ limit: 5 } },
      {...allCards.cardBattle_0008, ...{ limit: 1 } },
    ];
  } else {
    throw "unexpected seed: " + seed;
  }
  chAvailableCards(board, cards);
  // chSolution(board, ..);
}

function onServerMessage(event: MessageEvent): void {
  const serverMsg: ServerMessage = JSON.parse(event.data);
  switch (serverMsg.tag) {
    case "CurrentBoard": {
      return;
    }
  }
}