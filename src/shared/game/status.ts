/**
 * A status is a lingering effect on the gamestate.
 */
export type Status
  = Weak
  | Strong
  | Armor
  | Fragile
  ;

export class Weak {
  public readonly tag: "Weak" = "Weak";

  constructor(
    public readonly hp: number,
  ) {}
}

export class Strong {
  public readonly tag: "Strong" = "Strong";

  constructor(
    public readonly hp: number,
  ) {}
}

export class Armor {
  public readonly tag: "Armor" = "Armor";

  constructor(
    public readonly hp: number,
  ) {}
}

export class Fragile {
  public readonly tag: "Fragile" = "Fragile";

  constructor(
    public readonly hp: number,
  ) {}
}

export function statusGroup(
  status: Status,
) {
  switch (status.tag) {
    case "Armor": return "def_mod";
    case "Fragile": return "def_mod";
    case "Strong": return "atk_mod";
    case "Weak": return "atk_mod";
  }
}

export function statusMergeType(
  status: Status,
): "on_owner_id" {
  switch (status.tag) {
    case "Armor": return "on_owner_id";
    case "Fragile": return "on_owner_id";
    case "Strong": return "on_owner_id";
    case "Weak": return "on_owner_id";
  }
}

export type StatusTag = Status["tag"];

export const statusTags: StatusTag[]
  = ["Weak", "Strong", "Armor", "Fragile"];