export function clearPools<A extends {}>(
  screen: A,
) {
  const poolKeys = Object.keys(screen)
    .filter(x => x.endsWith("Pool"));
  poolKeys.forEach(poolKey => {
    ((screen as any)[poolKey]).clear();
  });
}