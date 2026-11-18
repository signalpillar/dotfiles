import test from "node:test";
import assert from "node:assert/strict";
import { normalizeUrl, isDedupable, isExcluded } from "./url.js";

const s = { stripHash: true, stripWww: true, ignoreTrailingSlash: true, stripParams: ["utm_*", "fbclid"] };

test("normalizes equivalent URLs to one key", () => {
  const a = normalizeUrl("https://www.example.com/path/?a=1&utm_source=x#top", s);
  const b = normalizeUrl("https://example.com/path?a=1", s);
  assert.equal(a, b);
});
test("keeps meaningful params and different paths distinct", () => {
  assert.notEqual(normalizeUrl("https://a.com/x?id=1", s), normalizeUrl("https://a.com/x?id=2", s));
  assert.notEqual(normalizeUrl("https://a.com/x", s), normalizeUrl("https://a.com/y", s));
});
test("root slash", () => assert.equal(normalizeUrl("https://a.com/", s), normalizeUrl("https://a.com", s)));
test("options off", () => {
  const o = { ...s, stripHash: false };
  assert.notEqual(normalizeUrl("https://a.com/#x", o), normalizeUrl("https://a.com/", o));
});
test("dedupable / excluded", () => {
  assert.equal(isDedupable("brave://settings"), false);
  assert.equal(isDedupable("https://a.com"), true);
  assert.equal(isExcluded("https://mail.google.com/compose", ["mail\\.google\\.com", "("]), true);
});
