// Pure helpers: turn an exercise definition into a flat list of timed laps.

export function expandLaps(ex) {
  const out = [];
  const sides = ex.sides ?? [null];
  const reps = ex.repeat ?? 1;
  if (ex.prep) out.push({ label: ex.prep[0], seconds: ex.prep[1], say: ex.prep[2] ?? ex.prep[0] });
  sides.forEach((side, si) => {
    if (si > 0) {
      out.push({
        label: `Switch to the ${side.toLowerCase()} side`,
        seconds: ex.switchSeconds ?? 4,
        say: `Switch. ${side} side.`,
        side,
      });
    }
    for (let rep = 1; rep <= reps; rep++) {
      ex.steps.forEach(([label, seconds, say], k) => {
        out.push({ label, seconds, say: say ?? label, rep, reps, side, first: k === 0 });
      });
    }
  });
  return out;
}

export const totalSeconds = (laps) => laps.reduce((sum, l) => sum + l.seconds, 0);

export function pickExercises(list, rotation, count) {
  const n = Math.min(count, list.length);
  return Array.from({ length: n }, (_, i) => list[(rotation + i) % list.length]);
}

export const todayKey = (d = new Date()) =>
  `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, "0")}-${String(d.getDate()).padStart(2, "0")}`;

export function formatClock(sec) {
  const s = Math.max(0, Math.ceil(sec));
  return `${Math.floor(s / 60)}:${String(s % 60).padStart(2, "0")}`;
}

export const minutesLeft = (nextAt, now = Date.now()) => Math.max(0, Math.ceil((nextAt - now) / 60000));
