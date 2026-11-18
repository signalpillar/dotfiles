export const DEFAULTS = {
  enabled: true,
  scope: "all", // "all" windows or "window" (same window only)
  dedupeOnNavigation: false, // also dedupe when an existing tab navigates to an open URL
  stripHash: true,
  stripWww: true,
  ignoreTrailingSlash: true,
  stripParams: ["utm_*", "fbclid", "gclid", "mc_cid", "mc_eid"],
  excludePatterns: [], // regex per entry, matched against the raw URL
};

export async function loadSettings() {
  const stored = await chrome.storage.sync.get(null);
  return { ...DEFAULTS, ...stored };
}
