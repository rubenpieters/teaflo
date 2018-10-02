import expect from "expect";
import { Solution, Path, SolutionIndex, nextIndex, initialIndex } from "src/shared/game/solution";

function basicTest() {
  const path1: Path = {
    restCard: { effects: [<any>undefined, <any>undefined], origin: { tag: "PlayerOrigin", cardId: 0 }, name: "", tag: "rest" },
    eventCards: [
      { event: { effects: [
        <any>undefined,
        <any>undefined,
      ], origin: { tag: "PlayerOrigin", cardId: 0 }, tag: "general", name: "" }, inputs: [] },
      { event: { effects: [<any>undefined], origin: { tag: "PlayerOrigin", cardId: 0 }, name: "", tag: "general" }, inputs: [] },
    ]
  }
  const path2: Path = {
    restCard: { effects: [<any>undefined], origin: { tag: "PlayerOrigin", cardId: 0 }, name: "", tag: "rest" },
    eventCards: [
      { event: { effects: [
        <any>undefined,
        <any>undefined,
      ], origin: { tag: "PlayerOrigin", cardId: 0 }, name: "", tag: "general" }, inputs: [] },
      { event: { effects: [<any>undefined], origin: { tag: "PlayerOrigin", cardId: 0 }, name: "", tag: "general" }, inputs: [] },
    ]
  }
  const solution: Solution = {
    paths: [path1, path2]
  }
  const index0: SolutionIndex = initialIndex;
  console.log("-- index1");
  const index1 = nextIndex(index0, solution);
  expect(index1).toEqual({
    path: 0,
    card: "rest",
    action: 1,
  });
  console.log("-- index2");
  const index2 = nextIndex((<SolutionIndex>index1), solution);
  expect(index2).toEqual({
    path: 0,
    card: 0,
    action: 0,
  });
  console.log("-- index3");
  const index3 = nextIndex((<SolutionIndex>index2), solution);
  expect(index3).toEqual({
    path: 0,
    card: 0,
    action: 1,
  });
  console.log("-- index4");
  const index4 = nextIndex((<SolutionIndex>index3), solution);
  expect(index4).toEqual({
    path: 0,
    card: 1,
    action: 0,
  });
  console.log("-- index5");
  const index5 = nextIndex((<SolutionIndex>index4), solution);
  expect(index5).toEqual({
    path: 1,
    card: "rest",
    action: 0,
  });
  console.log("-- index6");
  const index6 = nextIndex((<SolutionIndex>index5), solution);
  expect(index6).toEqual({
    path: 1,
    card: 0,
    action: 0,
  });
  console.log("-- index7");
  const index7 = nextIndex((<SolutionIndex>index6), solution);
  expect(index7).toEqual({
    path: 1,
    card: 0,
    action: 1,
  });
  console.log("-- index8");
  const index8 = nextIndex((<SolutionIndex>index7), solution);
  expect(index8).toEqual({
    path: 1,
    card: 1,
    action: 0,
  });
  console.log("-- index9");
  const index9 = nextIndex((<SolutionIndex>index8), solution);
  expect(index9).toEqual("done");
}

basicTest();