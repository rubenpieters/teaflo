import express from "express";
import path from "path";

main();

function main(): void {
  console.log("Server started");

  // parse port from env or default
  let parsedPort: number = 8080;
  const envPort: string | undefined = process.env["PORT"];
  if (envPort !== undefined) {
    parsedPort = Number(envPort);
  }

  // initialize server
  mkServer(parsedPort);
}

function mkServer(port: number): void {
  const distFolder: string = path.join(__dirname, "..", "..", "dist");
  const jsFolder: string = path.join(distFolder, "js");
  const texturesFolder: string = path.join(distFolder, "textures");
  const indexFile: string = path.join(distFolder, "index.html");

  const app = express();

  console.log(`dist folder: ${distFolder}`);

  app.get("/", (_req, res) => { res.sendFile(indexFile); });
  app.use("/js", express.static(jsFolder));
  app.use("/textures", express.static(texturesFolder));

  const server = app.listen(port, () => { console.log(`listening on ${port}`); });
}