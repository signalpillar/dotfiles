import { DEFAULTS, send } from "./state.js";
import { ARTICLE, ARRIVE, EXERCISES, RETURN } from "./exercises.js";
import { expandLaps, formatClock, pickExercises, totalSeconds } from "./laps.js";

const $ = (id) => document.getElementById(id);
const all = new URLSearchParams(location.search).has("all");
const state = await chrome.storage.local.get(DEFAULTS);
const picked = all ? EXERCISES : pickExercises(EXERCISES, state.rotation, state.perBreak);
const plan = [ARRIVE, ...picked, RETURN];

const laps = plan.flatMap((ex, i) => [
  { ex, i, prep: true, label: `Get ready: ${ex.title}`, say: `${ex.title}. ${ex.cue}`, seconds: i === 0 ? 4 : 7 },
  ...expandLaps(ex).map((l) => ({ ...l, ex, i })),
]);
const total = totalSeconds(laps);

let voice = state.voice;
let idx = -1;
let lapStart = 0;
let pausedAt = null;
let lastWhole = null;
let audio;
let shownEx = null;

// ---- sound ----
function tone(freq, dur = 0.18, gain = 0.12) {
  if (!audio) return;
  const t = audio.currentTime;
  const o = audio.createOscillator();
  const g = audio.createGain();
  o.type = "sine";
  o.frequency.value = freq;
  g.gain.setValueAtTime(0, t);
  g.gain.linearRampToValueAtTime(gain, t + 0.01);
  g.gain.exponentialRampToValueAtTime(0.0001, t + dur);
  o.connect(g).connect(audio.destination);
  o.start(t);
  o.stop(t + dur + 0.02);
}
const chime = () => { tone(660, 0.5); setTimeout(() => tone(880, 0.7), 140); };

function speak(text) {
  if (!voice || !text) return;
  speechSynthesis.cancel();
  const u = new SpeechSynthesisUtterance(text);
  u.rate = 1;
  u.pitch = 0.95;
  speechSynthesis.speak(u);
}

// ---- render ----
function setExercise(ex, i) {
  const isEx = !ex.mindful;
  $("position").textContent = isEx
    ? `Exercise ${picked.indexOf(ex) + 1} of ${picked.length}`
    : i === 0 ? "Begin · mindful pause" : "Close · mindful pause";
  $("title").textContent = ex.title;
  $("instructions").textContent = ex.instructions;
  $("benefit").textContent = ex.benefit ?? "";
  $("img").hidden = !isEx;
  $("breath").hidden = isEx;
  if (isEx) {
    $("img").src = ex.image;
    $("img").alt = ex.title;
  }
  $("credit").replaceChildren(
    ...(isEx
      ? ["Illustration: ", link(ARTICLE.publisher, ARTICLE.url)]
      : ["Breathe with the circle."]),
  );
  $("links").replaceChildren(
    ...(isEx
      ? [link("Read the exercise ↗", ARTICLE.url), link(`Why it helps · ${ex.source.label} ↗`, ex.source.url)]
      : []),
  );
}

function link(text, href) {
  const a = document.createElement("a");
  a.textContent = text;
  a.href = href;
  a.target = "_blank";
  a.rel = "noopener";
  return a;
}

const countsReps = (l) => !l.prep && !l.ex.mindful && l.reps > 1 && l.seconds < 5;

function describe(l) {
  const bits = [];
  if (countsReps(l)) bits.push(`of ${l.reps} reps`);
  else if (l.reps > 1) bits.push(`Rep ${l.rep} of ${l.reps}`);
  if (l.side) bits.push(`${l.side} side`);
  return bits.join(" · ");
}

function renderLap() {
  const l = laps[idx];
  if (shownEx !== l.ex) setExercise((shownEx = l.ex), l.i);
  $("lap").textContent = l.label;
  $("meta").textContent = l.prep ? "Get into position" : describe(l);
  if (countsReps(l)) $("count").textContent = l.rep;
  const n = laps[idx + 1];
  $("next").textContent = n ? `Then — ${n.ex !== l.ex ? n.ex.title : n.label}` : "Then — back to work";
  const b = $("breath");
  b.className = "breath" + (l.ex.mindful ? ` ${/in\b/i.test(l.label) ? "in" : /hold/i.test(l.label) ? "hold" : ""}` : "");
}

function startLap(i) {
  idx = i;
  lapStart = performance.now();
  lastWhole = null;
  const l = laps[i];
  const prev = laps[i - 1];
  if (l.prep) chime();
  else tone(prev && prev.ex === l.ex && l.side === prev.side ? 520 : 740);
  speak(l.first && l.reps > 1 && !l.prep ? String(l.rep) : l.say);
  renderLap();
}

function elapsedBefore(i) {
  let s = 0;
  for (let k = 0; k < i; k++) s += laps[k].seconds;
  return s;
}

function tick() {
  if (idx < 0 || pausedAt !== null) return;
  const l = laps[idx];
  const t = (performance.now() - lapStart) / 1000;
  const left = l.seconds - t;
  if (left <= 0) return idx + 1 < laps.length ? startLap(idx + 1) : finish();
  const whole = Math.ceil(left);
  if (whole !== lastWhole) {
    if (l.seconds >= 5 && whole <= 3 && lastWhole !== null) tone(440, 0.08, 0.07);
    lastWhole = whole;
  }
  if (!countsReps(l)) $("count").textContent = whole;
  $("lapBar").style.width = `${(t / l.seconds) * 100}%`;
  const done = elapsedBefore(idx) + t;
  $("overallBar").style.width = `${(done / total) * 100}%`;
  $("remaining").textContent = formatClock(total - done);
}

// ---- controls ----
function togglePause() {
  if (idx < 0) return;
  if (pausedAt === null) {
    pausedAt = performance.now();
    speechSynthesis.cancel();
  } else {
    lapStart += performance.now() - pausedAt;
    pausedAt = null;
  }
  document.body.classList.toggle("paused", pausedAt !== null);
  $("pause").textContent = pausedAt === null ? "Pause" : "Resume";
}

function skipLap() {
  if (idx < 0) return;
  if (pausedAt !== null) togglePause();
  idx + 1 < laps.length ? startLap(idx + 1) : finish();
}

function nextExercise() {
  if (idx < 0) return;
  if (pausedAt !== null) togglePause();
  const j = laps.findIndex((l, k) => k > idx && l.prep);
  j === -1 ? finish() : startLap(j);
}

function toggleVoice() {
  voice = !voice;
  if (!voice) speechSynthesis.cancel();
  $("voice").textContent = voice ? "Voice on" : "Voice off";
}

let finished = false;
function finish() {
  if (finished) return;
  finished = true;
  idx = -1;
  chime();
  speak("Well done. Back to work, one thing at a time.");
  $("overallBar").style.width = "100%";
  $("remaining").textContent = "0:00";
  $("doneTitle").textContent = `${formatClock(total)} of movement`;
  $("done").hidden = false;
  send({ type: "sessionDone", advance: all ? 0 : picked.length });
}

function begin() {
  audio ??= new AudioContext();
  audio.resume();
  $("intro").hidden = true;
  startLap(0);
}

$("introTitle").textContent = `${formatClock(total)} · ${picked.length} exercise${picked.length > 1 ? "s" : ""}`;
$("introList").replaceChildren(
  ...plan.map((ex) => {
    const li = document.createElement("li");
    const span = document.createElement("span");
    span.textContent = formatClock(totalSeconds(expandLaps(ex)));
    li.append(ex.title, span);
    return li;
  }),
);
$("voice").textContent = voice ? "Voice on" : "Voice off";
$("remaining").textContent = formatClock(total);

$("begin").onclick = begin;
$("pause").onclick = togglePause;
$("skip").onclick = skipLap;
$("skipEx").onclick = nextExercise;
$("voice").onclick = toggleVoice;
$("end").onclick = () => window.close();
$("close").onclick = () => window.close();

document.addEventListener("keydown", (e) => {
  if (e.target.closest("a")) return;
  if (!$("intro").hidden && (e.key === "Enter" || e.key === " ")) return e.preventDefault(), begin();
  if (e.key === " ") e.preventDefault(), togglePause();
  else if (e.key === "ArrowRight") skipLap();
  else if (e.key.toLowerCase() === "n") nextExercise();
  else if (e.key.toLowerCase() === "v") toggleVoice();
  else if (e.key === "Escape") window.close();
});

setInterval(tick, 100);
$("begin").focus();
