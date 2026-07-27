import fs from "node:fs";
import path from "node:path";

const root = process.argv[2];
if (!root) throw new Error("Usage: node audit-resource-links.mjs <resource-root>");

const catalog = JSON.parse(fs.readFileSync(path.join(root, "catalog.json"), "utf8"));
const links = [];
for (const recordPath of Object.values(catalog.topics)) {
  const topic = JSON.parse(fs.readFileSync(path.join(root, recordPath), "utf8"));
  links.push(...topic.links.map((link) => ({ topicId: topic.topicId, label: link.label, url: link.url })), {
    topicId: topic.topicId,
    label: topic.wikipedia.label,
    url: topic.wikipedia.url
  });
}

async function inspect(item) {
  const redirects = [];
  let url = item.url;
  let response;
  for (let count = 0; count < 10; count += 1) {
    response = await fetch(url, { redirect: "manual", headers: { "user-agent": "gcse-roadmap-link-audit/1" } });
    if (response.status < 300 || response.status >= 400) break;
    const location = response.headers.get("location");
    if (!location) break;
    const next = new URL(location, url).href;
    redirects.push(next);
    await response.body?.cancel();
    url = next;
  }
  await response?.body?.cancel();
  const likelyGate = /(?:login|log-in|sign-in|subscribe|checkout|payment|paywall)/i.test(url);
  return {
    ...item,
    redirects,
    finalUrl: url,
    status: response?.status ?? 0,
    contentType: response?.headers.get("content-type") || "",
    likelyAccountOrPaymentGate: likelyGate
  };
}

let failed = false;
for (const item of links) {
  try {
    const result = await inspect(item);
    console.log(JSON.stringify(result));
    if (result.status < 200 || result.status >= 400 || result.likelyAccountOrPaymentGate) failed = true;
  } catch (error) {
    failed = true;
    console.log(JSON.stringify({ ...item, error: error.message }));
  }
}
if (failed) process.exitCode = 1;
