export type ActData = {
  shortName: string,
  longName: string,
}

export const actData: {
  [key: number]: ActData
} = {
  0: {
    shortName: "1",
    longName: "Act 1",
  },
  1: {
    shortName: "2",
    longName: "Act 2",
  },
  2: {
    shortName: "3",
    longName: "Act 3",
  },
}