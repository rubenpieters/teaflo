import { changeConnected } from "src/app/appstate";

export function connectToServer(): void {
  const host: string = location.origin.replace(/^http/, "ws").replace(/localhost:3000/, "localhost:8080");
  const socket = new WebSocket(host);
  socket.onopen = () => {
    changeConnected("connected");
    socket.onmessage = onServerMessage;

    socket.send(JSON.stringify({ tag: "GetCurrentBoard" }));
  };
}

function onServerMessage(event: MessageEvent): void {
  console.log("received: " + event.data);
}