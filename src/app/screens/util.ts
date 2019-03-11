export function clearPools<A extends {}>(
  screen: A,
) {
  const poolKeys = Object.keys(screen)
    .filter(x => x.endsWith("Pool"));
  poolKeys.forEach(poolKey => {
    ((screen as any)[poolKey]).clear();
  });
}

export function clearAnimations<A extends {}>(
  game: Phaser.Game,
  screen: A,
) {
  const screenAny: any = screen as any;
  const poolKeys = Object.keys(screen)
    .filter(x => x.endsWith("Pool"));
  poolKeys.forEach(poolKey => {
    game.tweens.removeFrom(screenAny[poolKey], true);
    if (screenAny[poolKey].clearGroupAnimations !== undefined) {
      screenAny[poolKey].clearGroupAnimations();
    }
  });
}