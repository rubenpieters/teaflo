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
  public readonly hp: number;
  public readonly maxHp: number;

  constructor(
    public readonly value: number,
  ) {
    this.maxHp = statusModifier(this.tag) * value;
    this.hp = this.maxHp;
  }
}

export class Strong {
  public readonly tag: "Strong" = "Strong";
  public readonly hp: number;
  public readonly maxHp: number;

  constructor(
    public readonly value: number,
  ) {
    this.maxHp = statusModifier(this.tag) * value;
    this.hp = this.maxHp;
  }
}

export class Armor {
  public readonly tag: "Armor" = "Armor";
  public readonly hp: number;
  public readonly maxHp: number;

  constructor(
    public readonly value: number,
  ) {
    this.maxHp = statusModifier(this.tag) * value;
    this.hp = this.maxHp;
  }
}

export class Fragile {
  public readonly tag: "Fragile" = "Fragile";
  public readonly hp: number;
  public readonly maxHp: number;

  constructor(
    public readonly value: number,
  ) {
    this.maxHp = statusModifier(this.tag) * value;
    this.hp = this.maxHp;
  }
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

export function statusModifier(
  statusTag: StatusTag,
): number {
  switch (statusTag) {
    case "Armor": return 1;
    case "Fragile": return 1;
    case "Strong": return 7;
    case "Weak": return 7;
  }
}

export type StatusTag = Status["tag"];

export const statusTags: StatusTag[]
  = ["Weak", "Strong", "Armor", "Fragile"];