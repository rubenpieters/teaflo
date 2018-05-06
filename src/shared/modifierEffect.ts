export type IgnoreNextConsume = {
  tag: "IgnoreNextConsume",
}

export type IgnoreNextCheck = {
  tag: "IgnoreNextCheck",
}

export type ModifierEffect = IgnoreNextConsume | IgnoreNextCheck;