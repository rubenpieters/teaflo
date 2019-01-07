import expect from "expect";
import { extendTree, emptyTree, cutTree, getLocation } from "../../src/shared/tree";

export function basicTest() {
  let x = emptyTree();
  
  let t = extendTree((x, y) => x == y, x, [], 1);
  x = t.tree;
  expect(getLocation(x, [0])).toEqual(1);
  expect(t.loc).toEqual([0]);
  t = extendTree((x, y) => x == y, x, [0], 2);
  x = t.tree;
  expect(getLocation(x, [0, 0])).toEqual(2);
  expect(t.loc).toEqual([0, 0]);
  t = extendTree((x, y) => x == y, x, [0], 3);
  x = t.tree;
  expect(getLocation(x, [0, 1])).toEqual(3);
  expect(t.loc).toEqual([0, 1]);
  x = cutTree(x, [0]);
  expect(() => getLocation(x, [0, 0])).toThrow();
}

// basicTest();
