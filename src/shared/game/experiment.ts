import { mkGameState } from "./state";
import { HKT, URIS, Type } from "fp-ts/lib/HKT";
import { TargetId, GlobalId, UnitType } from "./entityId";
import * as fc from "fast-check";
import { focus, over } from "../iassign-util";
import { HasOwner } from "./trigger";
import { HasId } from "./hasId";
import deepEqual from "deep-equal";

const forCompilationOrder = mkGameState([], []);

declare module "fp-ts/lib/HKT" {
  interface URI2HKT<A> {
    Ability: IntentVar<A>,
    Action: A,
  }
}

type MY_URIS = Ability_URI | Action_URI;

export const Ability_URI: "Ability" = "Ability";
export type Ability_URI = typeof Ability_URI;

export const Action_URI: "Action" = "Action";
export type Action_URI = typeof Action_URI;

type IntentVar<A> = Static<A> | FromInput<A>;

export class Static<A> {
  readonly _A!: A
  readonly _URI!: Ability_URI

  public readonly tag: "Static" = "Static";

  constructor(
    public readonly a: A,
  ) {}
}

export class FromInput<A> {
  readonly _A!: A
  readonly _URI!: Ability_URI
  
  public readonly tag: "FromInput" = "FromInput";

  constructor(
    public readonly input: number,
  ) {}
}

class Damage<F extends URIS> {
  public readonly tag: "Damage" = "Damage";

  constructor(
    readonly uri: F,
    readonly value: Type<F, number>,
    readonly target: Type<F, TargetId>,
  ) {}
}

class UseCharge<F extends URIS> {
  public readonly tag: "UseCharge" = "UseCharge";

  constructor(
    readonly uri: F,
    readonly value: Type<F, number>,
    readonly target: Type<F, TargetId>,
  ) {}
}

export class Weak {
  public readonly tag: "Weak" = "Weak";

  constructor(
    public readonly fragments: number,
  ) {}
}

export class Strong {
  public readonly tag: "Strong" = "Strong";

  constructor(
    public readonly fragments: number,
  ) {}
}

export class Armor {
  public readonly tag: "Armor" = "Armor";

  constructor(
    public readonly fragments: number,
  ) {}
}

export class Fragile {
  public readonly tag: "Fragile" = "Fragile";

  constructor(
    public readonly fragments: number,
  ) {}
}

function statusGroup(
  status: Status,
) {
  switch (status.tag) {
    case "Armor": return "def_mod";
    case "Fragile": return "def_mod";
    case "Strong": return "atk_mod";
    case "Weak": return "atk_mod";
  }
}

function statusMergeType(
  status: Status,
): "on_owner_id" {
  switch (status.tag) {
    case "Armor": return "on_owner_id";
    case "Fragile": return "on_owner_id";
    case "Strong": return "on_owner_id";
    case "Weak": return "on_owner_id";
  }
}

type Status = Weak | Strong | Armor | Fragile;

const statusTags: Status["tag"][]
  = ["Weak", "Strong", "Armor", "Fragile"];

const tagArbitrary: fc.Arbitrary<Status["tag"]> = fc.constantFrom(...statusTags);

const fragmentsArbitrary: fc.Arbitrary<number> = fc.integer(0, 100000);

export const statusArbitrary: fc.Arbitrary<Status> = tagArbitrary.chain(tag => {
  return tagStatusArbitrary(tag)
});

function tagStatusArbitrary(
  tag: Status["tag"],
): fc.Arbitrary<Status> {
  switch (tag) {
    case "Strong": return fragmentsArbitrary.map(x => new Strong(x) as Status);
    case "Weak": return fragmentsArbitrary.map(x => new Weak(x));
    case "Armor": return fragmentsArbitrary.map(x => new Armor(x));
    case "Fragile": return fragmentsArbitrary.map(x => new Fragile(x));
  }
}

const statusIdNrArbitrary: fc.Arbitrary<number> = fc.integer(0, 20);

const statusIdArbitrary: fc.Arbitrary<GlobalId<"status">> =
  statusIdNrArbitrary.map(id => {
    return new GlobalId(id, "status");
  });

const unitIdNrArbitrary: fc.Arbitrary<number>
  = fc.integer(0, 3);
const unitTypeArbitrary: fc.Arbitrary<UnitType>
  = fc.constantFrom(...(["friendly", "enemy"] as UnitType[]));

const unitIdArbitrary: fc.Arbitrary<GlobalId<UnitType>> =
  unitTypeArbitrary.chain(unitType => {
    return unitIdNrArbitrary.map(id => new GlobalId(id, unitType));
  });

const stStatusArbitrary: fc.Arbitrary<Status & HasId & HasOwner> =
  fc.record({ status: statusArbitrary, id: statusIdNrArbitrary, owner: unitIdArbitrary })
  .map(r => { return {...r.status, id: r.id, owner: r.owner }});

class AetherRow {
  
  constructor(
    public readonly statuses: (Status & HasId & HasOwner)[] = [],
  ) {}

  damageFragments(
    statusId: GlobalId<"status">,
    value: number,
  ): AetherRow {
    const index = this.statuses.findIndex(x => x.id === statusId.id);
    if (index === -1) {
      // if not found, damage fizzles
      return this;
    }
    // reduce the fragment value of found index
    return new AetherRow(focus(this.statuses,
      over(x => x[index].fragments, x => Math.max(0, x - value)),
    ));
  }

  removeStatus(
    statusId: GlobalId<"status">,
  ) {
    return focus(this,
      over(x => x.statuses,
        x => x.filter(x => x.id !== statusId.id)),
    );
  }

  addStatus(
    status: Status,
    ownerId: GlobalId<UnitType>,
    nextId: () => number,
  ) {
    switch (statusMergeType(status)) {
      case "on_owner_id": {
        // merging on owner id
        const index = this.statuses.findIndex(x => deepEqual(x.owner, ownerId) && x.tag === status.tag);
        if (index === -1) {
          // if not found, create new status in row
          const newId = nextId();
          const newStatus: Status & HasOwner & HasId =
            {...status, owner: ownerId, id: newId };
          return focus(this,
              over(x => x.statuses, x => x.concat(newStatus))
            );
        } {
          // if found, add the fragment values to the existing status
          return focus(this,
            over(x => x.statuses[index].fragments,
              x => Math.max(0, x + status.fragments)),
          );
        }
      }
    }
  }
}

function aetherRowInvariant(
  row: AetherRow,
): boolean {
  for (const status of row.statuses) {
    switch (statusMergeType(status)) {
      case "on_owner_id": {
        const diff = row.statuses.filter(x => {
          return deepEqual(x.owner, status.owner) && x.tag === status.tag
        });
        // we expect only one status with
        // the same owner and tag: itself
        if (diff.length !== 1) return false;
      }
    }
  }
  return true;
}

const test1 = aetherRowInvariant(
  new AetherRow([
    {...new Weak(1), ...{ id: 1 }, ...{ owner: new GlobalId(1, "friendly") }},
    {...new Weak(2), ...{ id: 2 }, ...{ owner: new GlobalId(1, "friendly") }},
  ])
)

/*const aetherRowArbitrary: fc.Arbitrary<AetherRow> =
  fc.array(stStatusArbitrary).map(l => {
    return new AetherRow(l);
  });*/

const aetherRowArbitrary = concatArbitrary(statusTags.map(tag => {
  const unitIdListArb =
    fc.set(unitIdArbitrary, (a, b) => deepEqual(a, b))
  const statusListArb = unitIdListArb.chain(unitIdList => concatArbitrary(unitIdList.map(unitId => {
    return fc.record({ status: tagStatusArbitrary(tag), id: statusIdNrArbitrary })
    .map(r => {
      const st: Status & HasId & HasOwner = {...r.status, id: r.id, owner: unitId }
      return st;
    });
  })));
  return statusListArb;
})).map(l => {
  const flattened = l.reduce((acc, prev) => acc.concat(prev), []);
  return new AetherRow(flattened)
});

function concatArbitrary<A>(
  arbitraries: fc.Arbitrary<A>[],
): fc.Arbitrary<A[]> {
  return arbitraries.reduce((acc, arb) => {
    return acc.chain(l => arb.map(a => l.concat(a)));
  }, fc.constant([]) as fc.Arbitrary<A[]>);
}

fc.assert(fc.property(aetherRowArbitrary, (row) => deepEqual(row, row)));
fc.assert(fc.property(aetherRowArbitrary, (row) => aetherRowInvariant(row)));
fc.assert(fc.property(aetherRowArbitrary, statusIdArbitrary, fragmentsArbitrary,
  (row, statusId, value) => {
    const newRow = row.damageFragments(statusId, value);
    return aetherRowInvariant(newRow) &&
      // other elements are unchanged
      deepEqual(newRow.statuses.filter(x => x.id !== statusId.id), row.statuses.filter(x => x.id !== statusId.id))
      ;
  }));
fc.assert(fc.property(aetherRowArbitrary, statusArbitrary, unitIdArbitrary,
  (row, status, ownerId) => {
    const nextId = row.statuses.map(x => x.id).reduce((a,b) => a + b, 0);
    const newRow = row.addStatus(status, ownerId, () => nextId);
    return aetherRowInvariant(newRow) &&
      // the new status was added
      (newRow.statuses.filter(x => x.id === nextId).length === 1 ||
        // or, the status has 0 fragments
        status.fragments === 0 ||
        // or, the new status was merged into an existing status
        ! deepEqual(
          newRow.statuses.filter(x => x.tag === status.tag && deepEqual(x.owner, ownerId)),
          row.statuses.filter(x => x.tag === status.tag && deepEqual(x.owner, ownerId)),
        )
      )
      ;
  }));
fc.assert(fc.property(aetherRowArbitrary, statusIdArbitrary,
  (row, statusId) => {
    const newRow = row.removeStatus(statusId);
    return aetherRowInvariant(newRow) &&
      // no elements with this id are left
      newRow.statuses.filter(x => x.id === statusId.id).length === 0 &&
      // other elements are unchanged
      deepEqual(newRow.statuses.filter(x => x.id !== statusId.id), row.statuses.filter(x => x.id !== statusId.id))
      ;
  }));
