"""Pure HTML builders for the viewer (data in, HTML string out).

No disk reads, no harness imports, no request state.
Every task-derived string passes through `esc` before interpolation,
except text already rendered by the Markdown renderer.
"""

from __future__ import annotations

import html
from typing import TYPE_CHECKING

from markdown_it import MarkdownIt

if TYPE_CHECKING:
    from pi_job_viewer.store import BundleHandle, BundleSummary

_md = MarkdownIt("commonmark", {"html": False})


def esc(text: object) -> str:
    """Escape one value for HTML text or quoted attribute use."""
    return html.escape(str(text), quote=True)


def render_markdown(text: str) -> str:
    """Render Markdown to an HTML fragment (CommonMark, no raw-HTML passthrough worries)."""
    return _md.render(text or "")


STATUS_CLASS = {
    "planned": "pill-planned",
    "in_progress": "pill-progress",
    "blocked": "pill-blocked",
    "done": "pill-done",
    "skipped": "pill-skipped",
}


def pill(status: str) -> str:
    """Status badge span for a slice or bundle status value."""
    cls = STATUS_CLASS.get(status, "pill-planned")
    return f'<span class="pill {cls}">{esc(status)}</span>'


def crumb_trail(*items: tuple) -> str:
    """Breadcrumb trail: (label, href, hx_get) per item, current item last with Nones.

    Links swap via htmx like the rest of the app; the current page renders plain.
    """
    parts = []
    for label, href, hx_get in items:
        if href is None:
            parts.append(f"<span>{esc(label)}</span>")
        else:
            parts.append(
                f"<a href=\"{esc(href)}\" hx-get=\"{esc(hx_get or href)}\" "
                f"hx-target=\"#main\" hx-push-url=\"true\">{esc(label)}</a>"
            )
    return " / ".join(parts)


def shell(title: str, body: str, *, active: str = "") -> str:
    """Full page shell: Adwaita-style header bar, sidebar, htmx plus mermaid boot."""
    return f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{esc(title)} - pi-job viewer</title>
<link rel="stylesheet" href="/static/style.css">
<script src="https://unpkg.com/htmx.org@2.0.4/dist/htmx.min.js" integrity="sha384-HGfztofotfshcF7+8n44JQL2oJmowVChPTg48S+jvZoztPfvwD79OC/LTtG6dMp+" crossorigin="anonymous" defer></script>
<script src="https://cdn.jsdelivr.net/npm/mermaid@11.17.2/dist/mermaid.min.js" integrity="sha384-EOXBFmc3gx5mb+vn0vPvvGqACToJD24hhacX5Yx+8NUUQrHIle/Qi5Bg9o3zKwW2" crossorigin="anonymous" defer></script>
</head>
<body>
<header class="header-bar">
<span class="app-icon">◉</span>
<span class="app-title">pi-job viewer</span>
<nav class="crumbs" id="crumbs">{active}</nav>
</header>
<div class="layout">
<aside class="sidebar">
<a href="/" hx-get="/partial/bundles" hx-target="#main" hx-push-url="/">Bundles</a>
</aside>
<main id="main" class="content">{body}</main>
</div>
<script>
function fallbackCopy(text) {{
  var area = document.createElement("textarea");
  area.value = text;
  document.body.appendChild(area);
  area.select();
  try {{ document.execCommand("copy"); }} catch (err) {{}}
  document.body.removeChild(area);
}}
function renderMermaid() {{
  if (window.mermaid) {{
    mermaid.initialize({{ startOnLoad: false }});
    mermaid.run({{ querySelector: ".mermaid" }});
  }}
}}
document.addEventListener("DOMContentLoaded", renderMermaid);
document.body.addEventListener("htmx:afterSwap", renderMermaid);
document.body.addEventListener("click", function (e) {{
  var copyBtn = e.target.closest("[data-copy]");
  if (copyBtn) {{
    var text = copyBtn.getAttribute("data-copy");
    var done = function () {{
      copyBtn.textContent = "✓";
      setTimeout(function () {{ copyBtn.textContent = "⧉"; }}, 1200);
    }};
    if (navigator.clipboard && navigator.clipboard.writeText) {{
      navigator.clipboard.writeText(text).then(done, function () {{ fallbackCopy(text); done(); }});
    }} else {{
      fallbackCopy(text);
      done();
    }}
    return;
  }}
  var sortHead = e.target.closest("th[data-sort]");  if (sortHead) {{
    var table = sortHead.closest("table");
    var index = Array.prototype.indexOf.call(sortHead.parentNode.children, sortHead);
    var dir = sortHead.dataset.dir === "asc" ? "desc" : "asc";
    sortHead.dataset.dir = dir;
    var rows = Array.prototype.slice.call(table.tBodies[0].rows);
    rows.sort(function (a, b) {{
      var x = a.cells[index].innerText.trim();
      var y = b.cells[index].innerText.trim();
      return dir === "asc" ? x.localeCompare(y) : y.localeCompare(x);
    }});
    rows.forEach(function (r) {{ table.tBodies[0].appendChild(r); }});
    return;
  }}
  var btn = e.target.closest("[data-graph]");
  if (!btn) return;
  var wrap = btn.closest(".graph-wrap");
  var stage = wrap.querySelector(".graph-stage");
  var z = parseFloat(wrap.dataset.zoom || "1");
  var action = btn.getAttribute("data-graph");
  if (action === "zin") z = Math.min(z + 0.25, 3);
  if (action === "zout") z = Math.max(z - 0.25, 0.5);
  if (action === "zreset") z = 1;
  if (action === "expand") wrap.classList.toggle("expanded");
  wrap.dataset.zoom = z;
  stage.style.transform = "scale(" + z + ")";
  stage.style.transformOrigin = "top left";
}});
</script>
</body>
</html>"""


def bundle_list_fragment(bundles: list[BundleSummary]) -> str:
    """htmx fragment plus full-page body: sortable table of every bundle."""
    if not bundles:
        return '<p class="empty">No bundles found.</p>'
    rows = "".join(
        "<tr>"
        f"<td><a href=\"/b/{esc(b.slug)}/\" hx-get=\"/partial/bundles/{esc(b.slug)}\" "
        f"hx-target=\"#main\" hx-push-url=\"true\">{esc(b.title)}</a></td>"
        f"<td class=\"dim nowrap\">{esc(b.slug)}</td>"
        f"<td>{pill(b.status)}</td>"
        f"<td class=\"dim nowrap\">{esc(b.updated)}</td>"
        "</tr>"
        for b in bundles
    )
    return (
        "<table class='grid sortable'><thead><tr>"
        "<th data-sort='text'>Bundle</th><th data-sort='text'>Slug</th>"
        "<th data-sort='status'>Status</th><th data-sort='text'>Updated</th>"
        "</tr></thead><tbody>" + rows + "</tbody></table>"
    )


def _status_pill(sl: dict) -> str:
    """Status badge by effective status (running steps count as in_progress).

    The title names the stored status when it differs, so the page never
    rewrites what the store says.
    """
    from pi_job_viewer.store import effective_status

    stored = str(sl.get("status") or "")
    effective = effective_status(sl)
    pill_html = pill(effective)
    if effective != stored:
        pill_html = pill_html.replace('class="pill', f'title="stored: {esc(stored)}" class="pill', 1)
    return pill_html


def task_fragment(handle: BundleHandle, decisions_html: str, graph: str) -> str:
    """Task overview body: status-ranked slices table, decisions, graph."""
    from pi_job_viewer.store import ordered_slices, slice_activity

    ordered = ordered_slices(handle.task)
    rows = "".join(
        "<tr>"
        f"<td><a href=\"/b/{esc(handle.slug)}/slice/{esc(s.get('key'))}\" "
        f"hx-get=\"/partial/bundles/{esc(handle.slug)}/slices/{esc(s.get('key'))}\" hx-target=\"#main\" hx-push-url=\"true\">"
        f"{esc(s.get('title') or s.get('key'))}</a></td>"
        f"<td class=\"dim\"><code class=\"key\">{esc(s.get('key'))}</code>"
        f"<button class=\"copy\" data-copy=\"{esc(s.get('key'))}\" title=\"Copy slice key\">⧉</button></td>"
        f"<td class=\"dim\">{esc(s.get('kind') or '')}</td>"
        f"<td>{_status_pill(s)}</td>"
        f"<td class=\"dim\">{esc(slice_activity(s))}</td>"
        "</tr>"
        for s in ordered
    )
    table = (
        "<table class='grid sortable'><thead><tr>"
        "<th data-sort='text'>Slice</th><th data-sort='text'>Key</th><th data-sort='text'>Kind</th>"
        "<th data-sort='status'>Status</th><th data-sort='text'>Activity</th>"
        "</tr></thead><tbody>" + rows + "</tbody></table>"
    )
    return (
        f"<h1>{esc(handle.task.get('title') or handle.slug)}</h1>"
        f"<p>Status: {pill(handle.status)} "
        f"<span class='dim'>{esc(handle.slug)}</span> "
        f"<a href=\"/b/{esc(handle.slug)}/stats\" "
        f"hx-get=\"/partial/bundles/{esc(handle.slug)}/stats\" hx-target=\"#main\" hx-push-url=\"true\">Stats</a></p>"
        f"<h2>Slices</h2>{table}"
        f"{decisions_html}"
        f"{references_section(handle)}"
        "<h2>Graph</h2>"
        "<div class='graph-wrap'><div class='graph-tools'>"
        "<button data-graph='zin' title='Zoom in'>+</button>"
        "<button data-graph='zout' title='Zoom out'>-</button>"
        "<button data-graph='zreset' title='Reset zoom'>1:1</button>"
        "<button data-graph='expand' title='Expand overlay'>⤢</button>"
        "</div>"
        f"<div class='graph-stage'><pre class=\"mermaid\">{esc(graph)}</pre></div></div>"
    )


def repo_work_html(sl: dict) -> str:
    """Repo work section: worktree paths and PR links per repo, all copyable.

    Empty when the slice records no repo work.
    """
    repo_work = sl.get("repo_work") or {}
    if not repo_work:
        return ""
    blocks = []
    for repo in sorted(repo_work):
        entry = repo_work.get(repo) or {}
        worktree = str(entry.get("worktree") or "")
        if worktree:
            tree_html = (
                f"<code class=\"key\">{esc(worktree)}</code>"
                f"<button class=\"copy\" data-copy=\"{esc(worktree)}\" title=\"Copy worktree path\">⧉</button>"
            )
        else:
            tree_html = "<span class=\"dim\">not set</span>"
        prs = "".join(
            f"<li><a href=\"{esc(pr.get('url'))}\">{esc(pr.get('url'))}</a> "
            f"{pill(str(pr.get('status') or ''))} "
            f"<button class=\"copy\" data-copy=\"{esc(pr.get('url'))}\" title=\"Copy PR URL\">⧉</button></li>"
            for pr in (entry.get("prs") or [])
        )
        prs_html = f"<ul>{prs}</ul>" if prs else "<span class=\"dim\">no PRs</span>"
        blocks.append(f"<h3>{esc(repo)}</h3><p>Worktree: {tree_html}</p>{prs_html}")
    return "<h2>Repo work</h2>" + "".join(blocks)


def slice_fragment(slug: str, sl: dict, plan_html: str, root=None) -> str:
    """Slice detail body: goal, steps table with notes, related refs, plan file.

    Root is the bundle root; without it the related-references section stays empty.
    """
    steps = "".join(
        f"<tr><td class=\"dim nowrap\">{esc(st.get('key'))}</td>"
        f"<td>{esc(st.get('title') or '')}</td><td>{pill(str(st.get('status') or ''))}</td>"
        f"<td><div class=\"prose\">{render_markdown(str(st.get('note') or ''))}</div></td></tr>"
        for st in (*(sl.get("steps") or []), *(sl.get("final_steps") or []))
    )
    steps_table = (
        "<table class='grid sortable'><thead><tr>"
        "<th data-sort='text'>Step</th><th data-sort='text'>Title</th>"
        "<th data-sort='status'>Status</th><th>Note</th>"
        "</tr></thead><tbody>" + steps + "</tbody></table>"
    )
    return (
        f"<h1>{esc(sl.get('title') or sl.get('key'))}</h1>"
        f"<p>{pill(str(sl.get('status') or ''))} <span class='dim'>{esc(slug)} / {esc(sl.get('key'))}</span></p>"
        f"<p>{esc(sl.get('goal') or '')}</p>"
        f"<h2>Steps</h2>{steps_table}"
        f"{repo_work_html(sl)}"
        f"{related_references(root, slug, sl.get('key'))}"
        f"<h2>Plan</h2><div class='prose'>{plan_html}</div>"
    )


def decisions_table(records) -> str:
    """Sortable decisions table in a collapsed container, expanded on demand."""
    if not records:
        return "<p><em>_none_</em></p>"
    rows = "".join(
        "<tr>"
        f"<td class=\"dim nowrap\">{esc(record.date)}</td>"
        f"<td class=\"dim nowrap src\">{esc(record.source)}</td>"
        f"<td><div class=\"prose\">{render_markdown(record.body or record.claim)}</div></td>"
        "</tr>"
        for record in records
    )
    table = (
        "<table class='grid sortable'><thead><tr>"
        "<th data-sort='text'>Date</th><th data-sort='text'>Source</th><th>Decision</th>"
        "</tr></thead><tbody>" + rows + "</tbody></table>"
    )
    return f"<details class=\"collapsible\"><summary>Decisions ({len(records)})</summary>{table}</details>"


def file_link(slug: str, rel: str) -> str:
    """Link to a bundle file route: replaces the main view like all navigation."""
    return (
        f"<a href=\"/b/{esc(slug)}/{esc(rel)}\" "
        f"hx-get=\"/partial/bundles/{esc(slug)}/{esc(rel)}\" hx-target=\"#main\" hx-push-url=\"true\">"
        f"{esc(rel)}</a>"
    )


def references_section(handle) -> str:
    """Task files: rendered index map, the full `files` listing, unlinked gap list.

    In-bundle files link into the viewer and open rendered when Markdown.
    Out-of-bundle artifact paths show absolute with a copy button instead.
    The Unlinked subsection names reference files with no explicit frontmatter
    `slices:` link, so agents see what still needs linking.
    """
    from pi_job_viewer.store import bundle_file, unlinked_references

    index_html = ""
    try:
        index_html = f"<div class='prose'>{render_markdown(bundle_file(handle, 'references', 'index.md').read_text(encoding='utf-8', errors='replace'))}</div>"
    except OSError:
        index_html = ""
    items = "".join(
        f"<li>{file_link(handle.slug, display)}</li>"
        if browsable
        else (
            f"<li><code class=\"key\">{esc(display)}</code>"
            f"<button class=\"copy\" data-copy=\"{esc(display)}\" title=\"Copy path\">⧉</button> "
            "<span class=\"dim\">outside bundle</span></li>"
        )
        for display, browsable in handle.files
    )
    unlinked = "".join(
        f"<li>{file_link(handle.slug, rel)}</li>"
        for rel in unlinked_references(handle.root)
    )
    gap = f"<h3>Unlinked references</h3><ul>{unlinked}</ul>" if unlinked else ""
    return (
        f"<h2>References</h2>{index_html}<ul>{items}</ul>{gap}"
        if items or index_html
        else ""
    )


def related_references(root, slug: str, key: str) -> str:
    """Slice references: bundle files mentioning this slice key, else empty."""
    from pi_job_viewer.store import slice_references

    if root is None or not key:
        return ""
    matched = slice_references(root, str(key))
    if not matched:
        return ""
    items = "".join(f"<li>{file_link(slug, rel)}</li>" for rel in matched)
    return f"<h2>Related references</h2><ul>{items}</ul>"


def file_fragment(title: str, body_html: str) -> str:
    """Rendered plans/ or references/ file body with a heading."""
    return f"<h1>{esc(title)}</h1><div class='prose'>{body_html}</div>"
