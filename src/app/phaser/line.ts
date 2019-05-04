export function drawLine(
  graphics: Phaser.Graphics,
  start: { x: number, y: number },
  end: { x: number, y: number },
) {
  graphics.beginFill();
  graphics.lineStyle(4);
  graphics.moveTo(start.x, start.y);
  graphics.lineTo(end.x, end.y);
  graphics.endFill();
}