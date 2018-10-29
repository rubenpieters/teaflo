import expect from "expect";
import { extendTree, emptyTree, cutTree, getLocation } from "src/shared/tree";

function basicTest() {

  let x = emptyTree();

  x = extendTree((x, y) => x == y, x, [], 1);
  expect(getLocation(x, [0])).toBe(1);
  x = extendTree((x, y) => x == y, x, [0], 2);
  expect(getLocation(x, [0, 0])).toBe(2);
  x = extendTree((x, y) => x == y, x, [0], 3);
  expect(getLocation(x, [0, 1])).toBe(3);
  x = cutTree(x, [0]);
  expect(() => getLocation(x, [0, 0])).toThrow();
}

basicTest();
