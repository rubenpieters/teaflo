import { focus, over, set } from "src/shared/iassign-util";

export type Tree<A> = {
  nodes: Node<A>[],
}

export type Node<A> = {
  v: A,
  tree: Tree<A>,
}

export type Location = number[];

export function emptyTree<A>(
): Tree<A> {
  return {
    nodes: [],
  }
}

export function emptyNode<A>(
  a: A,
): Node<A> {
  return {
    v: a,
    tree: emptyTree(),
  }
}

export function extendTree<A>(
  check: (a1: A, a2: A) => boolean,
  tree: Tree<A>,
  loc: Location,
  a: A,
): {
  tree: Tree<A>,
  loc: Location,
} {
  return _extendTree(check, tree, loc, loc, a);
}

function _extendTree<A>(
  check: (a1: A, a2: A) => boolean,
  tree: Tree<A>,
  loc: Location,
  fullLoc: Location,
  a: A,
): {
  tree: Tree<A>,
  loc: Location,
} {
  if (loc.length === 0) {
    const checkResult = tree.nodes.findIndex(x => check(a, x.v));
    if (checkResult === -1) {
      return {
        tree: focus(tree, over(x => x.nodes, x => x.concat(emptyNode(a)))),
        loc: fullLoc.concat(tree.nodes.length),
      }
    } else {
      return {
        tree,
        loc: fullLoc.concat(checkResult),
      };
    }
  } else {
    const newValues = _extendTree(check, tree.nodes[loc[0]].tree, loc.slice(1), fullLoc, a);
    return {
      tree: focus(tree, set(x => x.nodes[loc[0]].tree, newValues.tree)),
      loc: newValues.loc,
    }
  }
}

export function cutTree<A>(
  tree: Tree<A>,
  loc: Location,
): Tree<A> {
  if (loc.length === 0) {
    return emptyTree();
  } else {
    return focus(tree,
      over(x => x.nodes[loc[0]].tree, x => cutTree(x, loc.slice(1))),
    );
  }
}

export function getLocation<A>(
  tree: Tree<A>,
  loc: Location,
): A {
  if (loc.length === 0) {
    throw `invalid location ${JSON.stringify(loc)}`;
  }
  const i = loc[0];
  if (i >= tree.nodes.length) {
    throw `invalid location ${JSON.stringify(loc)}`;
  }
  if (loc.length === 1) {
    return tree.nodes[i].v;
  }
  return getLocation(tree.nodes[i].tree, loc.slice(1));
}

export function drawPositions<A>(
  tree: Tree<A>,
): { a: A, x: number, y: number, loc: Location }[] {
  return _drawPositions(tree, 0, 0, [], []).result;
}

export function _drawPositions<A>(
  tree: Tree<A>,
  currentX: number,
  currentY: number,
  loc: Location,
  acc: { a: A, x: number, y: number, loc: Location }[],
): { maxY: number, result: { a: A, x: number, y: number, loc: Location }[] } {
  let newAcc = acc;
  tree.nodes.forEach((node, nodeIndex) => {
    const newLoc: Location = loc.concat(nodeIndex);
    newAcc = newAcc.concat({ a: node.v, x: currentX, y: currentY, loc: newLoc });
    const { maxY, result } = _drawPositions(node.tree, currentX + 1, currentY, newLoc, newAcc);
    currentY = maxY;
    newAcc = result;
  });
  if (tree.nodes.length === 0) {
    currentY += 1
  }
  return { maxY: currentY, result: newAcc };
}