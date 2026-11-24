import test from "node:test";
import assert from "node:assert/strict";
import { expandLaps, pickExercises, totalSeconds, formatClock, minutesLeft } from "./laps.js";
import { EXERCISES, ARRIVE } from "./exercises.js";

const byId = (id) => EXERCISES.find((e) => e.id === id);

test("calf raises: 15 reps of up/hold/down at 2s each", () => {
  const laps = expandLaps(byId("calf-raises"));
  assert.equal(laps.length, 45);
  assert.equal(totalSeconds(laps), 90);
  assert.deepEqual(laps.slice(0, 3).map((l) => l.say), ["Rise up onto your toes", "hold", "down"]);
  assert.equal(laps[3].rep, 2);
});

test("sided exercises insert a switch lap between sides", () => {
  const laps = expandLaps(byId("leg-extensions"));
  const switches = laps.filter((l) => l.label.startsWith("Switch"));
  assert.equal(switches.length, 1);
  assert.equal(switches[0].side, "Right");
  assert.equal(laps.filter((l) => l.side === "Left").length, 24);
});

test("calf stretch has a prep lap and 30s holds per side", () => {
  const laps = expandLaps(byId("calf-stretch"));
  assert.equal(laps[0].seconds, 5);
  assert.equal(totalSeconds(laps), 5 + 30 + 6 + 30);
});

test("box breathing is three 16s squares", () => {
  assert.equal(totalSeconds(expandLaps(ARRIVE)), 48);
});

test("pickExercises rotates and wraps", () => {
  assert.deepEqual(pickExercises([1, 2, 3, 4, 5, 6], 5, 2), [6, 1]);
  assert.deepEqual(pickExercises([1, 2, 3], 0, 6), [1, 2, 3]);
});

test("every exercise links its illustration and source", () => {
  assert.equal(EXERCISES.length, 6);
  for (const ex of EXERCISES) {
    assert.match(ex.image, /^https:\/\/cdn\.shopify\.com\/.+\.jpg$/, ex.id);
    assert.match(ex.source.url, /^https:\/\//, ex.id);
    assert.ok(totalSeconds(expandLaps(ex)) > 0, ex.id);
  }
  assert.equal(new Set(EXERCISES.map((e) => e.image)).size, 6);
});

test("formatting", () => {
  assert.equal(formatClock(75.2), "1:16");
  assert.equal(minutesLeft(10 * 60000 + 1, 0), 11);
  assert.equal(minutesLeft(-5, 0), 0);
});
