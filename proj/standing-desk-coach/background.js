import { DEFAULTS } from "./state.js";
import { minutesLeft, todayKey } from "./laps.js";

const get = () => chrome.storage.local.get(DEFAULTS);
const set = (patch) => chrome.storage.local.set(patch);
const ACCENT = "#a8432a";
const MUTED = "#8a857c";
const SNOOZE_MIN = 5;
const STAND_SNOOZE_MIN = 10;

async function rollDay(s) {
  const day = todayKey();
  if (s.day === day) return s;
  const patch = { day, breaksToday: 0, standingMsToday: 0, movedSecToday: 0 };
  if (s.standing) patch.standingSince = Date.now();
  await set(patch);
  return { ...s, ...patch };
}

async function ensureTick() {
  await chrome.alarms.create("tick", { periodInMinutes: 1 });
}

// Movement break while standing.
async function schedule(minutes) {
  const nextAt = Date.now() + minutes * 60000;
  await set({ nextAt, paused: false });
  await chrome.alarms.create("break", { when: nextAt });
  await ensureTick();
  await render();
}

// Stand-up reminder while sitting (0 = off).
async function scheduleStand(minutes) {
  await chrome.alarms.clear("stand");
  if (!minutes) {
    await set({ standAt: null, paused: false });
    await chrome.alarms.clear("tick");
    return render();
  }
  const standAt = Date.now() + minutes * 60000;
  await set({ standAt, paused: false });
  await chrome.alarms.create("stand", { when: standAt });
  await ensureTick();
  await render();
}

async function setStanding(on) {
  const s = await rollDay(await get());
  if (on === s.standing) return;
  if (on) {
    await chrome.alarms.clear("stand");
    await chrome.notifications.clear("stand");
    await set({ standing: true, standingSince: Date.now(), standAt: null });
    await schedule(s.intervalMin);
    return;
  }
  const ms = s.standingSince ? Date.now() - s.standingSince : 0;
  await set({ standing: false, standingSince: null, nextAt: null, standingMsToday: s.standingMsToday + ms });
  await chrome.alarms.clear("break");
  await chrome.notifications.clear("break");
  await scheduleStand(s.sitRemindMin);
}

function openSession(all = false) {
  chrome.notifications.clear("break");
  return chrome.windows.create({
    url: chrome.runtime.getURL(`session.html${all ? "?all=1" : ""}`),
    type: "popup",
    width: 980,
    height: 760,
    focused: true,
  });
}

async function prompt() {
  const s = await get();
  if (!s.standing) return;
  await schedule(s.intervalMin);
  if (s.delivery === "open") return openSession();
  await chrome.notifications.create("break", {
    type: "basic",
    iconUrl: await iconDataUrl(128, true),
    title: "Time to move",
    message: "A short, hands-free movement break. Press Begin and follow the voice.",
    buttons: [{ title: "Begin" }, { title: `Snooze ${SNOOZE_MIN} min` }],
    requireInteraction: true,
    priority: 2,
  });
}

async function promptStand() {
  const s = await get();
  if (s.standing || !s.sitRemindMin) return;
  await scheduleStand(s.sitRemindMin);
  await chrome.notifications.create("stand", {
    type: "basic",
    iconUrl: await iconDataUrl(128, false),
    title: "Time to stand up",
    message: `You've been sitting for ${s.sitRemindMin} minutes. Raise the desk, roll your shoulders, take a breath.`,
    buttons: [{ title: "I'm standing" }, { title: `Snooze ${STAND_SNOOZE_MIN} min` }],
    requireInteraction: true,
    priority: 2,
  });
}

function drawIcon(size, on) {
  const c = new OffscreenCanvas(size, size);
  const g = c.getContext("2d");
  const k = size / 32;
  g.scale(k, k);
  g.fillStyle = on ? ACCENT : MUTED;
  g.beginPath();
  g.roundRect(1, 1, 30, 30, 7);
  g.fill();
  g.fillStyle = g.strokeStyle = "#f6f2ea";
  g.lineWidth = on ? 2.4 : 2.6;
  g.lineCap = g.lineJoin = "round";
  g.beginPath();
  g.arc(on ? 16 : 13, on ? 8.25 : 7.5, on ? 2.9 : 3.2, 0, Math.PI * 2);
  g.fill();
  g.beginPath();
  if (on) {
    // Matches icon.svg: arms raised in a stretch.
    g.moveTo(16, 13); g.lineTo(16, 20.5);
    g.moveTo(11, 9.5); g.lineTo(16, 14.5); g.lineTo(21, 9.5);
    g.moveTo(13, 26.25); g.lineTo(16, 20.5); g.lineTo(19, 26.25);
  } else {
    g.moveTo(13, 12); g.lineTo(13, 20); g.lineTo(21, 20); g.lineTo(21, 27);
    g.moveTo(9, 13); g.lineTo(9, 27);
  }
  g.stroke();
  return c;
}

async function iconDataUrl(size, on) {
  const blob = await drawIcon(size, on).convertToBlob({ type: "image/png" });
  const bytes = new Uint8Array(await blob.arrayBuffer());
  let bin = "";
  for (const b of bytes) bin += String.fromCharCode(b);
  return `data:image/png;base64,${btoa(bin)}`;
}

async function render() {
  const s = await get();
  const imageData = Object.fromEntries(
    [16, 32].map((n) => [n, drawIcon(n, s.standing).getContext("2d").getImageData(0, 0, n, n)]),
  );
  await chrome.action.setIcon({ imageData });
  const due = s.standing ? s.nextAt : s.standAt;
  await chrome.action.setBadgeBackgroundColor({ color: s.standing ? ACCENT : MUTED });
  await chrome.action.setBadgeText({ text: s.paused ? "||" : due ? `${minutesLeft(due)}m` : "" });
  await chrome.action.setTitle({
    title: s.standing
      ? `Standing — next break in ${minutesLeft(s.nextAt ?? Date.now())} min`
      : s.standAt
        ? `Sitting — stand up in ${minutesLeft(s.standAt)} min`
        : "Sitting — reminders off",
  });
}

const handlers = {
  async toggle() {
    await setStanding(!(await get()).standing);
  },
  async settings({ patch }) {
    const before = await get();
    await set(patch);
    if (before.standing && patch.intervalMin && patch.intervalMin !== before.intervalMin) await schedule(patch.intervalMin);
    if (!before.standing && "sitRemindMin" in patch && patch.sitRemindMin !== before.sitRemindMin) {
      await scheduleStand(patch.sitRemindMin);
    }
  },
  start({ all }) {
    return openSession(all);
  },
  async sessionDone({ advance, seconds = 0 }) {
    const s = await rollDay(await get());
    await set({
      breaksToday: s.breaksToday + 1,
      movedSecToday: s.movedSecToday + seconds,
      rotation: (s.rotation + advance) % 6,
    });
    if (s.standing) await schedule(s.intervalMin);
  },
};

chrome.runtime.onMessage.addListener((msg, _sender, reply) => {
  Promise.resolve(handlers[msg.type]?.(msg)).then(() => reply({ ok: true }), (e) => reply({ ok: false, error: String(e) }));
  return true;
});

chrome.commands.onCommand.addListener((cmd) => cmd === "toggle-standing" && handlers.toggle());

chrome.alarms.onAlarm.addListener((a) => {
  if (a.name === "break") prompt();
  else if (a.name === "stand") promptStand();
  else render();
});

chrome.notifications.onClicked.addListener((id) => {
  if (id === "break") openSession();
  if (id === "stand") chrome.action.openPopup?.().catch(() => {});
});
chrome.notifications.onButtonClicked.addListener((id, i) => {
  chrome.notifications.clear(id);
  if (id === "break") i === 0 ? openSession() : schedule(SNOOZE_MIN);
  if (id === "stand") i === 0 ? setStanding(true) : scheduleStand(STAND_SNOOZE_MIN);
});

// Locking the screen means you've stepped away: pause, then restart the full interval on return.
chrome.idle.onStateChanged.addListener(async (state) => {
  const s = await get();
  if (state === "locked") {
    if (!s.nextAt && !s.standAt) return;
    await chrome.alarms.clear("break");
    await chrome.alarms.clear("stand");
    await set({ paused: true, nextAt: null, standAt: null });
    await render();
  } else if (state === "active" && s.paused) {
    if (s.standing) await schedule(s.intervalMin);
    else await scheduleStand(s.sitRemindMin);
  }
});

async function boot() {
  const s = await rollDay(await get());
  if (s.standing && !s.paused && !(await chrome.alarms.get("break"))) await schedule(1);
  if (!s.standing && !s.paused && s.sitRemindMin && !(await chrome.alarms.get("stand"))) {
    await scheduleStand(s.sitRemindMin);
  }
  if (s.nextAt || s.standAt) await ensureTick();
  await render();
}

chrome.runtime.onInstalled.addListener(boot);
chrome.runtime.onStartup.addListener(boot);
