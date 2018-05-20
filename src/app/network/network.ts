import { changeConnected, changeBoard } from "src/app/appstate";
import { ServerMessage } from "src/shared/network/serverMessage";
import { ClientMessage } from "src/shared/network/clientMessage";

export function connectToServer(): void {
  const host: string = location.origin.replace(/^http/, "ws").replace(/localhost:3000/, "localhost:8080");
  const socket = new WebSocket(host);
  socket.onopen = () => {
    changeConnected("connected");
    socket.onmessage = onServerMessage;
    const getBoardMessage: ClientMessage = {
      tag: "GetCurrentBoard",
      seed: "ABCD-EFGH",
    }

    socket.send(JSON.stringify(getBoardMessage));
  };
}

function onServerMessage(event: MessageEvent): void {
  const serverMsg: ServerMessage = JSON.parse(event.data);
  switch (serverMsg.tag) {
    case "CurrentBoard": {
      changeBoard(serverMsg.board);
      break;
    }
  }
}