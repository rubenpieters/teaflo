"use strict";

const WebSocket = require("uws");
const express = require('express');
const path = require('path');

exports.mkServer = function(config){
  return function() {
    const PORT = config.port;

    const DIST = path.join(__dirname, 'client', 'dist');
    const JS = path.join(DIST, 'client', 'js');
    const ASSETS = path.join(DIST, 'client', 'assets');
    const INDEX = path.join(DIST, 'client', 'index.html');
    console.log('dist folder: ' + DIST);

    const app = express();
    app.get('/', function(req, res) { res.sendFile(INDEX); });
    app.use('/js', express.static(JS));
    app.use('/assets', express.static(ASSETS));
    const server = app.listen(PORT, function() { console.log('Listening on ' + PORT); });

    const wss = new WebSocket.Server({ server: server });

    return wss;
  };
};

const unsafeSendMessage = function(data) {
  return function(client) {
    return function() {
      if (client.readyState === WebSocket.OPEN) {
        console.log("sending: " + data);
        client.send(data);
      }
    };
  };
};

exports.unsafeSendMessage = unsafeSendMessage;

exports.unsafeOn = function(event, cb, obj) {
  obj.on(event, cb);
};
