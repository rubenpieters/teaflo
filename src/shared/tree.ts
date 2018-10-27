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
): Tree<A> {
  if (loc.length === 0) {
    const checkResult = tree.nodes.filter(x => check(a, x.v));
    if (checkResult.length === 0) {
      return focus(tree,
        over(x => x.nodes, x => x.concat(emptyNode(a))),
      );
    } else {
      return tree;
    }
  } else {
    return focus(tree,
      over(x => x.nodes[loc[0]].tree, x => extendTree(check, x, loc.slice(1), a)),
    );
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

function getLocation<A>(
  tree: Tree<A>,
  loc: Location,
): A {
  if (loc.length === 0) {
    throw `invalid location ${JSON.stringify(loc)}`;
  }
  const i = loc[0];
  if (loc.length === 1) {
    return tree.nodes[i].v;
  }
  return getLocation(tree.nodes[i].tree, loc.slice(1));
}
