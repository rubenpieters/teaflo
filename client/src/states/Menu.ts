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

    const topSolText = 'Top Current Solutions';
    const playLocalText = 'Play Local Map';
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
    playLocal.events.onInputDown.add(this.startGame, this);

    let mapGen = this.add.text(0, 0, mapGenText, {
      font: '30px Indie Flower',
      fill: '#D3D3D3',
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });
    mapGen.setTextBounds(buttonMap['5'].xLeft, buttonMap['5'].yTop, buttonMap['5'].xWidth, buttonMap['5'].yHeight);

    let showTopHeight: number = buttonMap['3'].yHeight / showTopAmount;
    for (var i = 0; i <= showTopAmount; i++) {
      let showTop: Phaser.Text = this.add.text(0, 0, "x", {
      font: '20px Indie Flower',
      fill: '#D3D3D3',
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });
      showTop.setTextBounds(buttonMap['3'].xLeft, buttonMap['3'].yTop + i * showTopHeight, buttonMap['3'].xWidth, showTopHeight);

    }

    console.log("connecting to server");

    connectToServer();
  }

  render() {
  }

  startGame() {
    this.state.start('Game');
  }

}

async function connectToServer() {
  if (! connected) {
    //let HOST: string = location.origin.replace(/^https?/, 'ws')
    let HOST: string = 'ws://teaflo.herokuapp.com:42463'
    socket = new WebSocket(HOST);
    socket.onopen = function() {
      console.log("connected");
      socket.onmessage = function(msg) { PS.onServerStrMessageJS(msg.data)(); };
      connected = true;
    };
  } else {
    console.log("already connected!");
  }
}
