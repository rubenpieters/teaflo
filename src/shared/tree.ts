import { focus, over, set } from "src/shared/iassign-util";

type Tree<A> = {
  node: Node<A>[],
}

type Node<A> = {
  v: A,
  tree: Tree<A>,
}

type Location = number[];

export function emptyTree<A>(
): Tree<A> {
  return {
    node: [],
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
  tree: Tree<A>,
  loc: Location,
  a: A,
): Tree<A> {
  if (loc.length === 0) {
    return focus(tree,
      over(x => x.node, x => x.concat(emptyNode(a))),
    );
  } else {
    const traverseTree = tree.node[loc[0]].tree;
    return extendTree(traverseTree, loc.slice(1), a);
  }
}