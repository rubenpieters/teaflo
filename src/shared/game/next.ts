export type HasNext = {
  next: Next,
}

type NextId = {
  tag: "NextId",
}

type Repeat = {
  tag: "Repeat",
}

type Goto = {
  tag: "Goto",
  action: number,
}

export type Next
  = NextId
  | Repeat
  | Goto
