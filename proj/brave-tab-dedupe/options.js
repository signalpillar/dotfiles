import { DEFAULTS, loadSettings } from "./settings.js";

const bools = ["enabled", "dedupeOnNavigation", "stripHash", "stripWww", "ignoreTrailingSlash"];
const lists = ["stripParams", "excludePatterns"];
const $ = (id) => document.getElementById(id);

const s = await loadSettings();
for (const k of bools) $(k).checked = s[k];
for (const k of lists) $(k).value = s[k].join("\n");
$("scope").value = s.scope;

$("save").addEventListener("click", async () => {
  const next = { scope: $("scope").value };
  for (const k of bools) next[k] = $(k).checked;
  for (const k of lists) next[k] = $(k).value.split("\n").map((l) => l.trim()).filter(Boolean);
  await chrome.storage.sync.set({ ...DEFAULTS, ...next });
  $("status").textContent = "Saved";
  setTimeout(() => ($("status").textContent = ""), 1500);
});
