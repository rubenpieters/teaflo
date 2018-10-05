import { TargetType } from "src/shared/game/target";
import { InputEntityEffect } from "src/shared/game/ability";
import { CreatureId } from "./state";

export function showCard(card: Card) {
  //return { ...card, actions: card.actions.map(showAction) };
}

export type PlayerOrigin = {
  tag: "PlayerOrigin",
  cardId: number,
};

export type EntityOrigin = {
  tag: "EntityOrigin",
  entityId: number,
  entityType: TargetType,
};

export type CardOrigin
  = PlayerOrigin
  | CreatureId
  ;

export type Event = {
  tag: "crew" | "enemy" | "item" | "general",
  name: string,
  origin: CardOrigin,
  effects: InputEntityEffect[],
};

export type Rest = {
  tag: "rest",
  name: string,
  origin: CardOrigin,
  effects: InputEntityEffect[],
};

export type Card = Event | Rest;