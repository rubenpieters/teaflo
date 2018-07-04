export type Self = {
  tag: "Self",
}

export type AllCrew = {
  tag: "AllCrew",
}

export type TargetSpec
  = Self
  | AllCrew

export type Positions = {
  tag: "Positions",
  positions: number[],
}

export type TargetId = {
  tag: "TargetId",
  id: number,
}

export type Target
  = Positions
  | TargetId
  | AllCrew

export function findTarget(
  targetSpec: TargetSpec,
): Target {
  switch (targetSpec.tag) {
    case "Self": {
      throw "";
    }
    case "AllCrew": {
      return targetSpec;
    }
  }
}