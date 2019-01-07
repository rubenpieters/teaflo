export function createTween(
  game: Phaser.Game,
  obj: any,
  f: (tween: Phaser.Tween) => Phaser.Tween,
) {
  let tween = game.add.tween(obj);
  tween.frameBased = true;
  tween = f(tween);
  tween.onComplete.add(() => {
    game.tweens.remove(tween);
  });
  return tween;
}