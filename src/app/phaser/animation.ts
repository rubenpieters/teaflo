export function createTween(
  game: Phaser.Game,
  obj: any,
  f: (tween: Phaser.Tween) => void,
): Phaser.Tween {
  let tween = game.add.tween(obj);
  tween.frameBased = true;
  f(tween);
  tween.onComplete.add(() => {
    game.tweens.remove(tween);
  });
  return tween;
}

export function createChainedTween(
  game: Phaser.Game,
  obj: any,
  ...fs: ((tween: Phaser.Tween) => void)[]
): Phaser.Tween | undefined {
  let tween: Phaser.Tween | undefined = undefined;
  fs.forEach(f => {
    let t = game.add.tween(obj);
    t.frameBased = true;
    f(t);
    t.onComplete.add(() => {
      game.tweens.remove(t);
    });
    if (tween !== undefined) {
      tween = tween.chain(t);
    } else {
      tween = t;
    }
  });
  return tween;
}