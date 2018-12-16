import { emptyTree, extendTree, getLocation, Tree, drawPositions, cutTree } from "src/shared/tree";

const t0 = emptyTree();
const f = (a: any, b: any) => false;
const t1 = extendTree(f, t0, [], 1);
const t2 = extendTree(f, t1.tree, [0], 2);
const t3 = extendTree(f, t2.tree, [0, 0], 3);
const t4 = extendTree(f, t3.tree, [], 4);
console.log(JSON.stringify(drawPositions(t4.tree)));
const t5 = cutTree(t4.tree, [0]);
console.log(JSON.stringify(drawPositions(t5)));