const paramMatcher = (pattern) => {
  const re = new RegExp("^" + pattern.replace(/[.+?^${}()|[\]\\]/g, "\\$&").replace(/\*/g, ".*") + "$", "i");
  return (name) => re.test(name);
};

export function isDedupable(url) {
  return typeof url === "string" && /^https?:\/\//i.test(url);
}

export function isExcluded(url, patterns) {
  return patterns.some((p) => {
    try {
      return new RegExp(p).test(url);
    } catch {
      return false;
    }
  });
}

export function normalizeUrl(raw, s) {
  let u;
  try {
    u = new URL(raw);
  } catch {
    return raw;
  }
  if (s.stripHash) u.hash = "";
  if (s.stripWww) u.hostname = u.hostname.replace(/^www\./i, "");
  const matchers = (s.stripParams || []).filter(Boolean).map(paramMatcher);
  if (matchers.length) {
    for (const key of [...u.searchParams.keys()]) {
      if (matchers.some((m) => m(key))) u.searchParams.delete(key);
    }
  }
  let out = u.href;
  if (s.ignoreTrailingSlash) out = out.replace(/\/(?=($|\?|#))/, "");
  return out;
}
