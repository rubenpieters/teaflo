import express from "express";
import path from "path";
import uws from "uws";

import { ClientMessage } from "src/shared/network/clientMessage";
import { Board, generateBoard, boardData } from "src/shared/board";
import { newRng, rngHandler } from "src/shared/handler/rng/randomSeedRng";

main();

function main(): void {
  console.log("Server started");

  // parse port from env or default
  let parsedPort: number = 8080;
  const envPort: string | undefined = process.env.port;
  if (envPort !== undefined) {
    parsedPort = Number(envPort);
  }

  // initialize websocket server
  console.log("binding to port: " + parsedPort);
  const websocketServer: uws.Server = mkServer(parsedPort);

  // initialize db connection
  // TODO

  websocketServer.on("connection", onSocketConnection);
}

function mkServer(port: number): uws.Server {
  const distFolder: string = path.join(__dirname, "..", "..", "dist");
  const jsFolder: string = path.join(distFolder, "js");
  const indexFile: string = path.join(distFolder, "index.html");

  const app = express();

  console.log("dist folder: " + distFolder);

  app.get("/", (req, res) => { res.sendFile(indexFile); });
  app.use("/js", express.static(jsFolder));

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
        const board: Board = generateBoard(rngHandler(newRng(clientMsg.seed)), boardData);
        client.send(JSON.stringify({ tag: "CurrentBoard", board: board }));
        break;
      }
    }
  }
}