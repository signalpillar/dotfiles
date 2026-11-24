import { getState, send, INTERVALS, PER_BREAK, SIT_INTERVALS } from "./state.js";
import { minutesLeft, todayKey } from "./laps.js";
import { ARTICLE } from "./exercises.js";

const $ = (id) => document.getElementById(id);
$("article").href = ARTICLE.url;

function segment(el, values, fmt, key) {
  if (!el.children.length) el.innerHTML = values.map((v) => `<button data-v="${v}">${fmt(v)}</button>`).join("");
  el.onclick = (e) => {
    const b = e.target.closest("button");
    if (!b) return;
    const v = isNaN(Number(b.dataset.v)) ? b.dataset.v : Number(b.dataset.v);
    send({ type: "settings", patch: { [key]: v } });
  };
}

segment($("interval"), INTERVALS, (v) => `${v} min`, "intervalMin");
segment($("sitRemind"), SIT_INTERVALS, (v) => (v ? `${v} min` : "Off"), "sitRemindMin");
segment($("perBreak"), PER_BREAK, (v) => (v === 6 ? "All 6" : String(v)), "perBreak");
segment($("delivery"), [], String, "delivery");

const press = (el, value) => {
  for (const b of el.children) b.setAttribute("aria-pressed", String(b.dataset.v === String(value)));
};

async function render() {
  const s = await getState();
  const today = s.day === todayKey();
  $("standing").checked = s.standing;
  $("posture").textContent = s.standing ? "You're standing" : "You're sitting";
  $("status").textContent = s.paused
    ? "Paused while your screen was locked."
    : s.standing
      ? `A movement break every ${s.intervalMin} minutes.`
      : s.sitRemindMin
        ? `Breaks are off. A nudge to stand after ${s.sitRemindMin} minutes.`
        : "Breaks are off. Switch on when you stand.";
  const due = s.standing ? s.nextAt : s.standAt;
  $("nextBlock").hidden = !due;
  $("nextLabel").textContent = s.standing ? "Next break in" : "Stand up in";
  if (due) $("mins").textContent = minutesLeft(due);
  press($("interval"), s.intervalMin);
  press($("perBreak"), s.perBreak);
  press($("sitRemind"), s.sitRemindMin);
  press($("delivery"), s.delivery);
  $("voice").checked = s.voice;
  const live = s.standing && s.standingSince ? Date.now() - s.standingSince : 0;
  const mins = Math.round(((today ? s.standingMsToday : 0) + live) / 60000);
  $("breaks").textContent = today ? s.breaksToday : 0;
  $("stood").textContent = mins >= 60 ? `${Math.floor(mins / 60)} h ${mins % 60} min` : `${mins} min`;
}

$("standing").onchange = () => send({ type: "toggle" });
$("voice").onchange = (e) => send({ type: "settings", patch: { voice: e.target.checked } });
$("startNow").onclick = () => send({ type: "start", all: false }).then(() => window.close());
$("startAll").onclick = () => send({ type: "start", all: true }).then(() => window.close());

chrome.storage.onChanged.addListener(render);
setInterval(render, 15000);
render();
