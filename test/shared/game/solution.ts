import expect from "expect";
import { Solution, Path, SolutionIndex, nextIndex, initialIndex } from "src/shared/game/solution";
import { allCrew } from "src/shared/game/crew";

function basicTest() {
  const path1: Path = {
    restAction: { tag: "Rest" },
    cards: [
      [
        { tag: "Recruit", crew: allCrew.stFighter },
        { tag: "Recruit", crew: allCrew.stFighter },
      ],
      [{ tag: "Recruit", crew: allCrew.stFighter }],
    ]
  }
  const path2: Path = {
    restAction: { tag: "Rest" },
    cards: [
      [
        { tag: "Recruit", crew: allCrew.stFighter },
        { tag: "Recruit", crew: allCrew.stFighter },
      ],
      [{ tag: "Recruit", crew: allCrew.stFighter }],
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
    card: 0,
    action: 0,
  });
  console.log("-- index2");
  const index2 = nextIndex((<SolutionIndex>index1), solution);
  expect(index2).toEqual({
    path: 0,
    card: 0,
    action: 1,
  });
  console.log("-- index3");
  const index3 = nextIndex((<SolutionIndex>index2), solution);
  expect(index3).toEqual({
    path: 0,
    card: 1,
    action: 0,
  });
  console.log("-- index4");
  const index4 = nextIndex((<SolutionIndex>index3), solution);
  expect(index4).toEqual({
    path: 1,
    card: "rest",
    action: 0,
  });
  console.log("-- index5");
  const index5 = nextIndex((<SolutionIndex>index4), solution);
  expect(index5).toEqual({
    path: 1,
    card: 0,
    action: 0,
  });
  console.log("-- index6");
  const index6 = nextIndex((<SolutionIndex>index5), solution);
  expect(index6).toEqual({
    path: 1,
    card: 0,
    action: 1,
  });
  console.log("-- index7");
  const index7 = nextIndex((<SolutionIndex>index6), solution);
  expect(index7).toEqual({
    path: 1,
    card: 1,
    action: 0,
  });
  console.log("-- index8");
  const index8 = nextIndex((<SolutionIndex>index7), solution);
  expect(index8).toEqual("done");
}

basicTest();