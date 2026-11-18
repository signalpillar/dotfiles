import { loadSettings } from "./settings.js";
import { isDedupable, isExcluded, normalizeUrl } from "./url.js";

// Tabs created but not yet navigated to their first http(s) URL.
// Kept in session storage because the service worker can be killed at any time.
const FRESH_KEY = "freshTabs";
const closing = new Set();
let queue = Promise.resolve();

const getFresh = async () => new Set((await chrome.storage.session.get(FRESH_KEY))[FRESH_KEY] || []);
const setFresh = (set) => chrome.storage.session.set({ [FRESH_KEY]: [...set] });

async function markFresh(id) {
  const fresh = await getFresh();
  fresh.add(id);
  await setFresh(fresh);
}

async function unmarkFresh(id) {
  const fresh = await getFresh();
  if (fresh.delete(id)) await setFresh(fresh);
}

async function dedupe(tab) {
  const s = await loadSettings();
  if (!s.enabled) return;
  const url = tab.pendingUrl || tab.url;
  if (!isDedupable(url) || isExcluded(url, s.excludePatterns)) return;
  const key = normalizeUrl(url, s);
  const query = s.scope === "window" ? { windowId: tab.windowId } : {};
  const match = (await chrome.tabs.query(query)).find(
    (t) =>
      t.id !== tab.id &&
      !closing.has(t.id) &&
      t.incognito === tab.incognito &&
      isDedupable(t.url) &&
      normalizeUrl(t.url, s) === key,
  );
  if (!match) return;
  closing.add(tab.id);
  try {
    await chrome.tabs.update(match.id, { active: true });
    await chrome.windows.update(match.windowId, { focused: true });
    await chrome.tabs.remove(tab.id);
  } catch (e) {
    console.warn("tab-dedupe failed", e);
  } finally {
    closing.delete(tab.id);
  }
}

const enqueue = (fn) => (queue = queue.then(fn).catch((e) => console.warn(e)));

chrome.tabs.onCreated.addListener((tab) =>
  enqueue(async () => {
    await markFresh(tab.id);
    if (isDedupable(tab.pendingUrl || tab.url)) {
      await unmarkFresh(tab.id);
      await dedupe(tab);
    }
  }),
);

chrome.tabs.onUpdated.addListener((id, info, tab) => {
  if (!info.url) return;
  enqueue(async () => {
    const fresh = await getFresh();
    const s = await loadSettings();
    if (fresh.has(id) && isDedupable(info.url)) {
      await unmarkFresh(id);
      await dedupe(tab);
    } else if (s.dedupeOnNavigation && !fresh.has(id)) {
      await dedupe(tab);
    }
  });
});

chrome.tabs.onRemoved.addListener((id) => enqueue(() => unmarkFresh(id)));
