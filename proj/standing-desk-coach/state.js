export const DEFAULTS = {
  standing: false,
  paused: false,
  intervalMin: 30,
  sitRemindMin: 50,
  standAt: null,
  perBreak: 2,
  voice: true,
  delivery: "notify",
  nextAt: null,
  rotation: 0,
  standingSince: null,
  standingMsToday: 0,
  breaksToday: 0,
  day: "",
};

export const INTERVALS = [20, 30, 45, 60];
export const PER_BREAK = [2, 3, 6];
export const SIT_INTERVALS = [0, 30, 45, 50, 60];

export const getState = () => chrome.storage.local.get(DEFAULTS);
export const send = (msg) => chrome.runtime.sendMessage(msg);
