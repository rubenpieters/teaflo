import { emptyTree, extendTree, getLocation, Tree } from "src/shared/tree";

const t0 = emptyTree();
const f = (a: any, b: any) => false;
const t1 = extendTree(f, t0, [], 1);
console.log(JSON.stringify(t1));
console.log(getLocation(t1.tree, [0]));
const t2 = extendTree(f, t1.tree, [], 2);
console.log(JSON.stringify(t2));
console.log(getLocation(t2.tree, [1]));
const t3 = extendTree(f, t2.tree, [0], 3);
console.log(JSON.stringify(t3));
console.log(getLocation(t3.tree, [0, 0]));
const t4 = extendTree(f, t3.tree, [0, 0], 4);
console.log(JSON.stringify(t4));
console.log(getLocation(t4.tree, [0, 0, 0]));
const t5 = extendTree(f, t4.tree, [0], 5);
console.log(JSON.stringify(t5));
console.log(getLocation(t5.tree, [0, 1]));