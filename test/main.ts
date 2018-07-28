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
  },
): TestResult {
  const acceptNew = options !== undefined && options.acceptNew !== undefined ? options.acceptNew : false;
  let goldenFileCreated;
  let testSucceeded;

  if (! acceptNew && fs.existsSync(path.resolve(__dirname, goldenFile))) {
    goldenFileCreated = false;

    const goldenContents = fs.readFileSync(path.resolve(__dirname, goldenFile), { encoding: "utf8" });
    if (goldenContents === testResult) {
      testSucceeded = true;
    } else {
      testSucceeded = false;
    }
  } else {
    fs.writeFileSync(path.resolve(__dirname, goldenFile), testResult, { encoding: "utf8" });
    testSucceeded = true;
    goldenFileCreated = true;
  }

  return { goldenFileCreated, testSucceeded };
}

const testResult = goldenTest("golden_files/test1.txt", "1");
console.log(testResult);