// In-memory Nostr relay for tests. Supports EVENT, REQ, CLOSE, EOSE, filters with tags,
// since, until and limit, replaceable (10000-19999) and addressable (30000-39999) events.
// It verifies signatures with the same nostr-tools bundle the pages load.
const fs = require("node:fs");
const path = require("node:path");
const { WebSocketServer } = require("ws");

const bundle = path.join(__dirname, "fixtures", "nostr.bundle.js");
const NostrTools = new Function(`${fs.readFileSync(bundle, "utf8")}\n;return NostrTools;`)();

const isReplaceable = (kind) => (kind >= 10000 && kind < 20000) || kind === 0 || kind === 3;
const isAddressable = (kind) => kind >= 30000 && kind < 40000;
const dTag = (event) => (event.tags.find((t) => t[0] === "d") || [])[1] || "";
const newer = (a, b) => a.created_at > b.created_at || (a.created_at === b.created_at && a.id < b.id);

async function startFakeRelay() {
  const events = [];
  const sockets = new Set();
  let down = false;
  const server = new WebSocketServer({ port: 0, host: "127.0.0.1" });
  await new Promise((resolve) => server.once("listening", resolve));
  const url = `ws://127.0.0.1:${server.address().port}`;

  function store(event) {
    if (events.some((e) => e.id === event.id)) return "duplicate:";
    const sameSlot = (e) => e.pubkey === event.pubkey && e.kind === event.kind && (isReplaceable(event.kind) || dTag(e) === dTag(event));
    if (isReplaceable(event.kind) || isAddressable(event.kind)) {
      const index = events.findIndex(sameSlot);
      if (index !== -1) {
        if (!newer(event, events[index])) return "duplicate: have a newer version";
        events.splice(index, 1);
      }
    }
    events.push(event);
    return "";
  }

  function query(filter) {
    const matching = events.filter((e) => NostrTools.matchFilter(filter, e)).sort((a, b) => b.created_at - a.created_at || (a.id < b.id ? -1 : 1));
    return filter.limit !== undefined ? matching.slice(0, filter.limit) : matching;
  }

  server.on("connection", (ws) => {
    if (down) { ws.terminate(); return; }
    sockets.add(ws);
    const subs = new Map();
    ws.on("close", () => sockets.delete(ws));
    ws.on("message", (data) => {
      let msg;
      try { msg = JSON.parse(data.toString()); } catch { return; }
      if (msg[0] === "EVENT") {
        const event = msg[1];
        if (!NostrTools.verifyEvent(event)) { ws.send(JSON.stringify(["OK", event.id, false, "invalid: bad signature"])); return; }
        const note = store(event);
        ws.send(JSON.stringify(["OK", event.id, true, note]));
        if (note) return;
        for (const client of sockets) {
          for (const [subId, filters] of client.subs || []) {
            if (filters.some((f) => NostrTools.matchFilter(f, event))) client.send(JSON.stringify(["EVENT", subId, event]));
          }
        }
      } else if (msg[0] === "REQ") {
        const [, subId, ...filters] = msg;
        subs.set(subId, filters);
        const sent = new Set();
        for (const filter of filters) {
          for (const event of query(filter)) {
            if (sent.has(event.id)) continue;
            sent.add(event.id);
            ws.send(JSON.stringify(["EVENT", subId, event]));
          }
        }
        ws.send(JSON.stringify(["EOSE", subId]));
      } else if (msg[0] === "CLOSE") {
        subs.delete(msg[1]);
      }
    });
    ws.subs = subs;
  });

  return {
    url,
    events,
    NostrTools,
    // Adds events directly, as if another device had published them.
    inject(list) { for (const event of list) store(event); },
    eventsFor(label) { return events.filter((e) => e.tags.some((t) => t[0] === "l" && t[1] === label)); },
    // Drops every connection and refuses new ones, like a relay that is offline.
    goDown() { down = true; for (const ws of sockets) ws.terminate(); },
    comeUp() { down = false; },
    close() { for (const ws of sockets) ws.terminate(); return new Promise((resolve) => server.close(resolve)); }
  };
}

module.exports = { startFakeRelay, NostrTools };
