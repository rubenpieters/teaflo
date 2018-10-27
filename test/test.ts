import { extendTree, emptyTree, cutTree } from "src/shared/tree";


let x = emptyTree();

x = extendTree((x, y) => x == y, x, [], 1);
console.log(JSON.stringify(x));
x = extendTree((x, y) => x == y, x, [0], 2);
console.log(JSON.stringify(x));
x = extendTree((x, y) => x == y, x, [0], 3);
console.log(JSON.stringify(x));
x = cutTree(x, [0]);
console.log(JSON.stringify(x));