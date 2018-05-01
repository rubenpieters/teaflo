export type ResourceColor = "Basic" | "Red" | "Green" | "Blue" | "Yellow" | "Victory";
export type ResourceType = "Temp" | "Total";

export type Resource = {
  color: ResourceColor,
  type: ResourceType,
  amount: number,
}