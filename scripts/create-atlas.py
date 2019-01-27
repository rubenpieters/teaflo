import os

atlasNames = ["test"]

for atlas in atlasNames:
  print(f"creating atlas {atlas}")
  os.system(f"texpack --output ../assets/{atlas} --POT {atlas}.txt")

