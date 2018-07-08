import { changeConnected } from "src/app/appstate";
import { changeAvailableCards } from "src/app/gamestate";
import { Card } from "src/shared/game/solution";
import { allCrew } from "src/shared/game/crew";
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

export function getBoard(serverConn: ServerConnection, seed: string) {
  /*const getBoardMessage: ClientMessage = {
    tag: "GetCurrentBoard",
    seed: seed,
  };

  serverConn.socket.send(JSON.stringify(getBoardMessage));*/
  const cards: Card[] = [
    [{ tag: "Recruit", crew: allCrew.stFighter }],
    [{ tag: "Recruit", crew: allCrew.stRanged }],
  ];
  changeAvailableCards(cards);
}

function onServerMessage(event: MessageEvent): void {
  const serverMsg: ServerMessage = JSON.parse(event.data);
  switch (serverMsg.tag) {
    case "CurrentBoard": {
      return;
    }
  }
}