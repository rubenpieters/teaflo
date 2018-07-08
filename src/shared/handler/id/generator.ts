export type Generator = {
  newId: () => number
};

export function plusOneGenerator() {
  return {
    newId: generateId({ x: 0 })
  };
}

function generateId(ref: { x: number }) {
  return function() {
    const val = ref.x;
    ref.x += 1;
    return val;
  };
}