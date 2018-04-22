import express from "express";
import path from "path";
import uws from "uws";

import { ClientMessage } from "src/shared/network/clientMessage";
import { Board, generateBoard, boardData } from "src/shared/board";
import { rng } from "src/shared/handler/rng/randomSeedRng";

main();

function main(): void {
  console.log("Server started");

  // parse port from env or default
  // TODO: get port from Env
  const parsedPort: number = 8080;

  // initialize websocket server
  console.log("binding to port: " + parsedPort);
  const websocketServer: uws.Server = mkServer(parsedPort);

  // initialize db connection
  // TODO

  websocketServer.on("connection", onSocketConnection);
}

function mkServer(port: number): uws.Server {
  const distFolder: string = path.join(__dirname, "dist");
  const indexFile: string = path.join(distFolder, "index.html");

  const app = express();

  console.log("dist folder: " + distFolder);

  app.get("/", (req, res) => { res.sendFile(indexFile); });
  app.use("/js", express.static(distFolder));

  const server = app.listen(port, () => { console.log("listening on " + port); });

  const websocketServer: uws.Server = new uws.Server({ server: server });

  return websocketServer;
}

function onSocketConnection(client: uws) {
  console.log("Player Connect");
  client.on("message", onClientMessage(client));
}

function onClientMessage(client: uws) { 
  return function(msg: string) {
    console.log("received " + msg);
    const clientMsg: ClientMessage = JSON.parse(msg);
    switch (clientMsg.tag) {
      case "GetCurrentBoard": {
        const board: Board = generateBoard(rng, boardData);
        client.send(JSON.stringify({ tag: "CurrentBoard", board: board }));
        break;
      }
    }
  }
}