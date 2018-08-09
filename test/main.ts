import iassign from "immutable-assign";
import * as fs from "fs";
import * as path from "path";

type TestResult = {
  goldenFileCreated: boolean,
  testSucceeded: boolean,
}

function goldenTest(
  goldenFile: string,
  testResult: string,
  options?: {
    acceptNew?: boolean,
    errorOnCreate?: boolean,
  },
): TestResult {
  const acceptNew = options !== undefined && options.acceptNew !== undefined ? options.acceptNew : false;
  const errorOnCreate = options !== undefined && options.errorOnCreate !== undefined ? options.errorOnCreate : false;
  let goldenFileCreated;
  let testSucceeded;

  if (fs.existsSync(path.resolve(__dirname, goldenFile))) {
    goldenFileCreated = false;

    if (acceptNew) {
      fs.writeFileSync(path.resolve(__dirname, goldenFile), testResult, { encoding: "utf8" });
      testSucceeded = true;
    } else {
      const goldenContents = fs.readFileSync(path.resolve(__dirname, goldenFile), { encoding: "utf8" });
      if (goldenContents === testResult) {
        testSucceeded = true;
      } else {
        testSucceeded = false;
      }
    }
  } else {
    if (errorOnCreate) {
      testSucceeded = false;
      goldenFileCreated = false;
    } else {
      fs.writeFileSync(path.resolve(__dirname, goldenFile), testResult, { encoding: "utf8" });
      testSucceeded = true;
      goldenFileCreated = true;
    }
  }

  return { goldenFileCreated, testSucceeded };
}

const testResult = goldenTest("golden_files/test1.txt", "1");
console.log(testResult);