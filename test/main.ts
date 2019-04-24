import iassign from "immutable-assign";
import expect from "expect";

import * as TreeTest from "./shared/tree";
import * as a2l1_sol from "./acts/a2l1_sol";
import { goldenTest } from "./golden";

console.log("Running Tests");

console.log("Tree Test Basic Test");
TreeTest.basicTest();

console.log("A2L1 Golden Test");
expect(
    goldenTest("golden_files/a2l1_sol", a2l1_sol.run,
    { acceptNew: false }
  ).testSucceeded)
  .toBe(true);
