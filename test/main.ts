import iassign from "immutable-assign";
import expect from "expect";
import * as fs from "fs";
import * as path from "path";

type TestResult = {
  goldenFileCreated: boolean,
  testSucceeded: boolean,
}

function goldenTest(
  goldenFile: string,
  testResult: () => string,
  options?: {
    acceptNew?: boolean,
    errorOnCreate?: boolean,
  },
): TestResult {
  const acceptNew = options !== undefined && options.acceptNew !== undefined ? options.acceptNew : false;
  const errorOnCreate = options !== undefined && options.errorOnCreate !== undefined ? options.errorOnCreate : false;
  let goldenFileCreated;
  let testSucceeded;

  // TODO: catch exceptions ?
  let testResultForced = testResult();

  if (fs.existsSync(path.resolve(__dirname, goldenFile))) {
    goldenFileCreated = false;

    if (acceptNew) {
      fs.writeFileSync(path.resolve(__dirname, goldenFile), testResultForced, { encoding: "utf8" });
      testSucceeded = true;
    } else {
      const goldenContents = fs.readFileSync(path.resolve(__dirname, goldenFile), { encoding: "utf8" });
      if (goldenContents === testResultForced) {
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
      fs.writeFileSync(path.resolve(__dirname, goldenFile), testResultForced, { encoding: "utf8" });
      testSucceeded = true;
      goldenFileCreated = true;
    }
  }

  return { goldenFileCreated, testSucceeded };
}
