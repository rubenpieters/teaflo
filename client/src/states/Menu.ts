import * as Phaser from 'phaser-ce';

import * as PS from 'js/purs.bundle';

const buttonMap = PS.buttonMap();

var connected = false;
var socket;

const showTopAmount = 3;

export default class extends Phaser.State {
  init() { }
  preload() { }

  create() {
    console.log(buttonMap);

    let bannerText = 'TeaFlo';
    let banner = this.add.text(0, 0, bannerText, {
      font: '60px Indie Flower',
      fill: '#77BFA3',
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });
    banner.setTextBounds(buttonMap['1'].xLeft, buttonMap['1'].yTop, buttonMap['1'].xWidth, buttonMap['1'].yHeight);

    const topSolText = 'Play Current Board';
    const playLocalText = 'Play Local Board';
    const mapGenText = 'Map Generator';

    let topSol = this.add.text(0, 0, topSolText, {
      font: '30px Indie Flower',
      fill: '#D3D3D3',
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });
    topSol.setTextBounds(buttonMap['2'].xLeft, buttonMap['2'].yTop, buttonMap['2'].xWidth, buttonMap['2'].yHeight);

    let playLocal = this.add.text(0, 0, playLocalText, {
      font: '30px Indie Flower',
      fill: '#77BFA3',
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });
    playLocal.setTextBounds(buttonMap['4'].xLeft, buttonMap['4'].yTop, buttonMap['4'].xWidth, buttonMap['4'].yHeight);

    playLocal.inputEnabled = true;
    playLocal.events.onInputOver.add(function() { playLocal.fill = '#88CFB4'; playLocal.setShadow(1.5,1.5,'rgb(0,0,0,0.15)', 3); }, this);
    playLocal.events.onInputOut.add(function() { playLocal.fill = '#77BFA3'; playLocal.setShadow(null);}, this);
    playLocal.events.onInputDown.add(this.startGame(undefined), this);

    let mapGen = this.add.text(0, 0, mapGenText, {
      font: '30px Indie Flower',
      fill: '#D3D3D3',
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });
    mapGen.setTextBounds(buttonMap['5'].xLeft, buttonMap['5'].yTop, buttonMap['5'].xWidth, buttonMap['5'].yHeight);

    let showTopHeight: number = buttonMap['3'].yHeight / showTopAmount;
    let showTopX: Phaser.Text[] = [];
    for (var i = 0; i <= showTopAmount - 1; i++) {
      let showTop: Phaser.Text = this.add.text(0, 0, "x", {
        font: '20px Indie Flower',
        fill: '#D3D3D3',
        boundsAlignH: "center",
        boundsAlignV: "middle",
      });
      showTop.setTextBounds(buttonMap['3'].xLeft, buttonMap['3'].yTop + i * showTopHeight, buttonMap['3'].xWidth, showTopHeight);
      showTopX.push(showTop);
    }

    console.log("connecting to server");

    connectToServer(this.setCurrentTop(showTopX, showTopAmount), this.setCurrentBoard(topSol, this));
  }

  render() {
  }

  startGame(board) {
    return function() {
      this.state.start('Game', true, false, board);
    }
  }

  setCurrentTop(topX: Phaser.Text[], topAmount: number) {
    return function(data) {
      let top: number[] = data.top;
      return function() {
        for (var i = 0; i <= topAmount - 1; i++) {
          topX[i].setText("- Branch from: " + top[i] + " (x VP)");
        }
      }
    }
  }

  setCurrentBoard(playCurrent, t) {
    return function(data) {
      let board = data.board;
      return function() {
        playCurrent.fill = '#77BFA3';
        playCurrent.inputEnabled = true;
        playCurrent.events.onInputOver.add(function() { playCurrent.fill = '#88CFB4'; playCurrent.setShadow(1.5,1.5,'rgb(0,0,0,0.15)', 3); }, t);
        playCurrent.events.onInputOut.add(function() { playCurrent.fill = '#77BFA3'; playCurrent.setShadow(null);}, t);
        playCurrent.events.onInputDown.add(t.startGame(board), t);
      }
    }
  }

}

async function connectToServer(onGetCurrentTop, onGetCurrentBoard) {
  if (! connected) {
    let HOST: string = location.origin.replace(/^http/, 'ws').replace(/localhost:3000/, 'localhost:8080')
    console.log(HOST);
    socket = new WebSocket(HOST);
    socket.onopen = function() {
      console.log("connected");
      socket.onmessage = function(msg) { PS.onServerStrMessageJS({ onGetCurrentTop: onGetCurrentTop, onGetCurrentBoard: onGetCurrentBoard })(msg.data)(); };
      connected = true;
    };
  } else {
    console.log("already connected!");
  }
}
