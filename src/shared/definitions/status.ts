import { Ability } from "./ability";
import { StatusAbility } from "./statusTransform";

export type StatusGroup
  = "atk_mod"
  | "def_mod"
  | "other"
  ;

export type StatusTag = Status["tag"];

/**
 * A status is a lingering effect on the gamestate.
 */
export type Status
  = Weak
  | Strong
  | Armor
  | Fragile
  | OnDeath
  ;

export class Weak {
  public readonly tag: "Weak" = "Weak";
  public readonly maxHp: number;
  public readonly hp: number;

  constructor(
    public readonly value: number,
    hp?: number,
  ) {
    this.maxHp = statusModifier(this.tag) * value;
    if (hp !== undefined) {
      this.hp = hp;
    } else {
      this.hp = this.maxHp;
    }
  }
}

export class Strong {
  public readonly tag: "Strong" = "Strong";
  public readonly maxHp: number;
  public readonly hp: number;

  constructor(
    public readonly value: number,
    hp?: number,
  ) {
    this.maxHp = statusModifier(this.tag) * value;
    if (hp !== undefined) {
      this.hp = hp;
    } else {
      this.hp = this.maxHp;
    }
  }
}

export class Armor {
  public readonly tag: "Armor" = "Armor";
  public readonly maxHp: number;
  public readonly hp: number;

  constructor(
    public readonly value: number,
    hp?: number,
  ) {
    this.maxHp = statusModifier(this.tag) * value;
    if (hp !== undefined) {
      this.hp = hp;
    } else {
      this.hp = this.maxHp;
    }
  }
}

export class Fragile {
  public readonly tag: "Fragile" = "Fragile";
  public readonly maxHp: number;
  public readonly hp: number;

  constructor(
    public readonly value: number,
    hp?: number,
  ) {
    this.maxHp = statusModifier(this.tag) * value;
    if (hp !== undefined) {
      this.hp = hp;
    } else {
      this.hp = this.maxHp;
    }
  }
}

export class OnDeath {
  public readonly tag: "OnDeath" = "OnDeath";
  public readonly maxHp: number;
  public readonly hp: number;

  constructor(
    public readonly ability: StatusAbility,
    public readonly value: number,
    hp?: number,
  ) {
    this.maxHp = statusModifier(this.tag) * value;
    if (hp !== undefined) {
      this.hp = hp;
    } else {
      this.hp = this.maxHp;
    }
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
    // TODO: find a fitting value
    // this should depend on the content of the OnDeath status
    case "OnDeath": return 10;
  }
}