import iassign from "immutable-assign";
import expect from "expect";

import * as TreeTest from "./shared/tree";
import * as a1l1_sol from "./acts/a1l1_sol";
import { goldenTest } from "./golden";

console.log("Running Tests");

console.log("Tree Test Basic Test");
TreeTest.basicTest();

console.log("A1L1 Golden Test");
expect(
    goldenTest("golden_files/a1l1_sol", a1l1_sol.run,
    { acceptNew: false }
  ).testSucceeded)
  .toBe(true);
