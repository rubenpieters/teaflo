export type HasNext = {
  next: Next,
}

type NextId = {
  tag: "NextId"
}

type Repeat = {
  tag: "Repeat"
}

export type Next
  = NextId
  | Repeat
