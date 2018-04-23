import { Node } from "src/shared/node";

type AddConnectionAction = {
  tag: "AddConnectionAction",
  from: Node,
  to: Node,
}

export type Action = AddConnectionAction;

export type History = Action[];

