# Nostr storage for prototypes

This note records the design of the shared Nostr layer for the prototypes in this directory.
It also records why we took each decision.
Read it before you change `store.js` or `account.js`, or before you connect a new prototype.

## Goal

A prototype keeps working on local data.
When you sign in with a Nostr key, the same data syncs between your devices through public relays.
One shared component handles sign-in, keys and relays.
Each prototype declares its own data model in its manifest, and the shared code does the rest.

## Decisions

| # | Decision | Reason |
|---|----------|--------|
| 1 | Only you read and write your data, on many devices. | One writer gives the simplest trust model. Encryption to your own key works. Sharing with other people is out of scope for v1. |
| 2 | Sign-in with a pasted or generated `nsec`, or with a NIP-07 browser extension. | `nsec` works everywhere, also in tests. NIP-07 keeps the key out of the page on desktop. NIP-46 remote signers come later. |
| 3 | One key for all prototypes. | GitHub Pages serves all prototypes from one origin, so they share browser storage. You sign in once. An app label in each event keeps the data of each prototype apart. |
| 4 | Encrypt all content with NIP-44 to your own key. | Public relays show everything. Relays still see the author, the time, the kind and the tags. A collection can set `"public": true` to skip encryption. |
| 5 | Relay list: your NIP-65 list (kind 10002), else the library defaults. A prototype can override it. | Every prototype and every other Nostr client reads the same list. You edit it once in the account drawer. |
| 6 | Two collection types: `record` and `log`. | A record is mutable, and the last write wins (settings, scorecards). A log entry never changes, and logs merge by id (the fact-graph log). |
| 7 | The model is declared in the `storage` block of the prototype manifest. | This follows the one-manifest rule in `AGENTS.md`. The shared code validates the block. |
| 8 | Local-first: the page reads and writes a local copy in IndexedDB at once, and sync runs in the background. | The page stays fast and works offline. A retry queue publishes pending writes. |
| 9 | Without a key, a prototype works on local data only. At the first sign-in, the anonymous data moves to that key and uploads once. | No sign-in wall on the first visit. |
| 10 | Each key has its own local copy. Anonymous data is a separate copy. | A switch of key never mixes data, and never uploads one person's data under another key. |
| 11 | The account UI is one custom element, `<nostr-account>`: a badge that opens a drawer. | One tag gives every prototype the same sign-in, backup, relay and sync controls. |
| 12 | Inline media (the 480 px image recipes) is deferred to v2. | The first adopter, fact-graph, has no media. |
| 13 | fact-graph syncs its fact log only. | Node positions, the open tab and replay are per device. Sample data never uploads. |
| 14 | Default relays: `wss://relay.damus.io`, `wss://nos.lol`, `wss://relay.primal.net`. | On 2026-10-20, all three accepted a NIP-44 encrypted kind 30078 event and returned it for a `#l` tag filter. |

## Event layout

Every item is one kind 30078 event (NIP-78, application data, addressable).

```json
{
  "kind": 30078,
  "tags": [
    ["d", "<appId>/<collection>/<itemId>"],
    ["L", "signalpillar.prototypes"],
    ["l", "<appId>/<collection>", "signalpillar.prototypes"]
  ],
  "content": "<NIP-44 ciphertext of the JSON body, or the plain JSON body when public>"
}
```

The JSON body is `{"v": 1, "value": {...}, "updatedAt": <ms>, "deleted": false}`.

- The `d` tag makes the event addressable, so a newer record event replaces the older one on the relay.
- Relays index single-letter tags, so a `#l` filter fetches one collection without fetching the other apps.
- The `L`/`l` pair follows NIP-32 labels with our own namespace.
- A log entry uses the same layout. The shared code never publishes a second event for the same log id.
- A delete publishes the same `d` tag with `"deleted": true`. NIP-09 deletion is optional for relays, so we do not rely on it.
- `updatedAt` is in milliseconds, inside the encrypted body. It orders writes more finely than `created_at`, which is in seconds.

## Relay limits

These values come from the NIP-11 documents on 2026-10-20.

| Relay | `max_message_length` | `max_limit` | `max_subscriptions` |
|-------|----------------------|-------------|---------------------|
| relay.damus.io | 1 000 000 | 500 | 200 |
| nos.lol | 131 072 | 500 | 100 |
| relay.primal.net | 1 000 000 | 500 | 20 |

- A query returns at most 500 events, so the first pull pages backward with `until`.
- The publish queue sends one event at a time, with a short pause, to stay under relay rate limits.

## Prototype contract

A prototype adds three classic scripts and one element.
Classic scripts also load from `file://`, where ES module imports fail.

```html
<script src="https://cdn.jsdelivr.net/npm/nostr-tools@2.25.2/lib/nostr.bundle.js" defer></script>
<script src="nostr/store.js" defer></script>
<script src="nostr/account.js" defer></script>
...
<nostr-account></nostr-account>
```

It declares its model in its manifest:

```json
"storage": {
  "appId": "fact-graph",
  "collections": {
    "log": {
      "type": "log",
      "localOnlyIdPrefix": "seed-",
      "fields": { "id": "string", "kind": "string", "at": "string" },
      "required": ["id", "kind", "at"]
    }
  }
}
```

- `type` is `record` or `log`.
- `fields` maps a field name to `string`, `number`, `boolean`, `object` or `array`. The shared code rejects a local write that breaks it and drops a remote item that breaks it.
- `required` lists the fields that must be present.
- `localOnlyIdPrefix` marks ids that never upload, for example sample data.
- `public: true` publishes plain JSON instead of ciphertext.
- `relays` (optional, on the `storage` block) replaces the relay list for this prototype.

It uses the API:

```js
const store = await NostrPrototypes.openStore(manifest.storage);
store.list("log");                       // items of the current owner, from memory
await store.append("log", entry);        // log: a new id; a known id is ignored
await store.put("settings", record);     // record: record.id is the key, the last write wins
await store.remove("settings", id);      // record: publishes a deleted marker
const off = store.subscribe((change) => render()); // change.reason: "local" | "remote" | "owner"
document.querySelector("nostr-account").store = store; // badge shows this store's sync state
```

## Sync

1. `openStore` validates the manifest block and opens the local copy of the current owner.
   The owner is `anon` or the public key in hex.
2. Every write goes to IndexedDB first.
   When the owner is a key and the id is not local-only, the write also goes to the outbox.
3. The publisher takes one outbox row at a time.
   It encrypts, signs and publishes the row to the write relays.
   It deletes the row after one relay answers `OK true`.
   On failure, it waits and tries again, with a longer wait each time.
4. The puller fetches each collection by its `#l` label, from the last sync time, and pages with `until`.
   After that, one live subscription receives new events.
5. A remote log entry with an unknown id is added.
   A remote record replaces the local one when its `updatedAt` is newer.
6. At sign-in, the anonymous items move to the key's copy and enter the outbox.
   At sign-out, the page shows the anonymous copy again.

## Security limits

- A pasted or generated `nsec` stays in `localStorage`.
  Any script on the same origin can read it.
  Use it for prototype data only, or use a browser extension.
- Encryption hides the content, not the metadata: author, time, app name and collection name stay visible.
- Relays can drop events at any time. Export stays the backup.
- If you use your main Nostr identity, a relay change in the drawer also changes the relay list of your other Nostr clients.

## Tests

- `prototype.tests/nostr/` holds a fake relay (`fake-relay.js`) and browser tests of the shared code.
  The fake relay keeps events in memory and supports `EVENT`, `REQ`, `CLOSE`, `EOSE`, tag filters, `since`, `until`, `limit` and addressable replacement.
- Tests never use the public relays.
  A test signs in with a fixed key and sets the relay list to the fake relay.
- `prototype.tests/fact-graph/` tests the fact-graph adoption with two browser contexts as two devices.
