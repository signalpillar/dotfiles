"""E2E viewer tests: real bundles via the pi-job CLI, real HTTP via TestClient.

Each test proves one observable page contract.
No unit matrix replays the harness validators; the harness owns those.
"""

from __future__ import annotations

import os
import shutil
import subprocess

import pytest
from fastapi.testclient import TestClient

from pi_job_viewer.app import create_app

PI_JOB_BIN = shutil.which("pi-job") or os.path.expanduser("~/.local/bin/pi-job")


def _run_viewer_cli(env: dict, *args: str) -> None:
    """Run the installed pi-job CLI with a fixture tasks home."""
    clean = {k: v for k, v in env.items() if k != "PI_JOB_OWNER"}
    proc = subprocess.run([PI_JOB_BIN, *args], env=clean, capture_output=True, text=True, check=False)
    assert proc.returncode == 0, proc.stderr


@pytest.fixture()
def home(tmp_path, monkeypatch):
    """Two real bundles under an isolated tasks home."""
    tasks = tmp_path / "tasks"
    tasks.mkdir()
    env = dict(os.environ, PI_JOB_TASKS=str(tasks))
    monkeypatch.setenv("PI_JOB_TASKS", str(tasks))
    _run_viewer_cli(env, "--task", "alpha", "create", "--goal", "Ship alpha outcome")
    _run_viewer_cli(env, "--task", "beta", "create", "--goal", "Ship beta outcome")
    (tasks / "alpha" / "plans" / "do-the-change.md").write_text("# Alpha plan\n\nShip it.\n", encoding="utf-8")
    return tasks


@pytest.fixture()
def client(home):
    """TestClient bound to the fixture tasks home."""
    return TestClient(create_app(home))


def test_index_lists_bundles(client):
    """GET / names every bundle slug."""
    res = client.get("/")
    assert res.status_code == 200
    assert "alpha" in res.text
    assert "beta" in res.text


def test_task_page_shows_slices_decisions_graph(client):
    """GET /b/alpha/ carries the slice key and a Mermaid graph."""
    res = client.get("/b/alpha/")
    assert res.status_code == 200
    assert "do-the-change" in res.text
    assert "flowchart LR" in res.text


def test_slice_page_renders_plan(client):
    """GET slice page embeds the rendered slice plan body."""
    res = client.get("/b/alpha/slice/do-the-change")
    assert res.status_code == 200
    assert "Ship it." in res.text


@pytest.mark.parametrize("path", ["/b/nope/", "/b/alpha/plans/../../task.yaml", "/b/alpha/slice/nope"])
def test_unknown_paths_are_404(client, path):
    """Unknown slugs, slices, and out-of-root files share one 404."""
    assert client.get(path).status_code == 404


def test_raw_html_in_plan_is_escaped(client, home):
    """Plan bodies never pass raw HTML through to the page."""
    (home / "alpha" / "plans" / "do-the-change.md").write_text("<script>alert(1)</script>\n", encoding="utf-8")
    res = client.get("/b/alpha/slice/do-the-change")
    assert res.status_code == 200
    assert "&lt;script&gt;" in res.text


def test_javascript_link_has_no_anchor(client, home):
    """Plan links with dangerous schemes never become clickable anchors."""
    (home / "alpha" / "plans" / "do-the-change.md").write_text("[click](javascript:alert(1))\n", encoding="utf-8")
    res = client.get("/b/alpha/slice/do-the-change")
    assert res.status_code == 200
    assert 'href="javascript' not in res.text


def test_skin_served(client):
    """GET /static/style.css returns the Adwaita skin."""
    res = client.get("/static/style.css")
    assert res.status_code == 200
    assert "header-bar" in res.text


def test_shell_rerenders_mermaid_after_swap(client):
    """Full pages boot Mermaid on load and after every htmx swap."""
    res = client.get("/")
    assert res.status_code == 200
    assert "htmx:afterSwap" in res.text
    assert 'class="mermaid"' not in res.text or "mermaid.run" in res.text


def test_decisions_render_as_markdown(client, home):
    """Decision notes render Markdown instead of flat escaped text."""
    env = dict(os.environ, PI_JOB_TASKS=str(home))
    _run_viewer_cli(env, "--task", "alpha", "add-decision", "--date", "2026-09-28", "--note", "Ship **boldly**.", "--source", "chat:test")
    res = client.get("/b/alpha/")
    assert res.status_code == 200
    assert "<strong>boldly</strong>" in res.text


def test_decisions_sort_newest_first(client, home):
    """Decisions list newest dates before older ones."""
    env = dict(os.environ, PI_JOB_TASKS=str(home))
    _run_viewer_cli(env, "--task", "beta", "add-decision", "--date", "2026-09-20", "--note", "Older note", "--source", "chat:test")
    _run_viewer_cli(env, "--task", "beta", "add-decision", "--date", "2026-09-28", "--note", "Newer note", "--source", "chat:test")
    res = client.get("/b/beta/")
    assert res.status_code == 200
    assert res.text.index("Newer note") < res.text.index("Older note")
    assert res.text.count("data-sort='text'>Date") == 1


@pytest.mark.parametrize("path", ["/partial/bundles", "/partial/bundles/alpha"])
def test_partial_urls_render_styled_without_htmx(client, path):
    """Direct visits to partial URLs get the full styled shell."""
    res = client.get(path)
    assert res.status_code == 200
    assert "stylesheet" in res.text
    assert "header-bar" in res.text


def test_partial_urls_return_fragments_for_htmx(client):
    """htmx swaps get bare fragments without the shell."""
    res = client.get("/partial/bundles/alpha", headers={"hx-request": "true"})
    assert res.status_code == 200
    assert "stylesheet" not in res.text
    assert "flowchart LR" in res.text


def test_quiet_read_uses_store_suppression_hook():
    """Viewer reads engage the suppression hook instead of warning on stderr."""
    from contextlib import contextmanager

    from pi_job_viewer.store import _quiet_read

    used = []

    class Hooked:
        @contextmanager
        def _suppress_digest_warn(self):
            used.append(True)
            yield

    with _quiet_read(Hooked()):
        pass
    assert used == [True]


def test_quiet_read_passes_stores_without_hook_through():
    """Stores without the hook read as-is instead of failing."""
    from pi_job_viewer.store import _quiet_read

    with _quiet_read(object()):
        pass


def test_topo_order_puts_dependencies_first():
    """Topological order delivers dependencies before dependents."""
    from pi_job_viewer.store import topo_order

    keys = ["c", "a", "b"]
    edges = {"c": ["a", "b"], "a": [], "b": ["a"]}
    assert topo_order(keys, edges) == ["a", "b", "c"]


def test_effective_status_rank_orders_running_work_first():
    """A planned slice with running steps ranks with in_progress; display untouched."""
    from pi_job_viewer.store import effective_status, status_rank

    assert status_rank(effective_status({"status": "planned", "steps": [{"status": "in_progress"}]})) == 0
    assert status_rank(effective_status({"status": "planned", "steps": []})) == 1
    assert status_rank(effective_status({"status": "blocked"})) == 2
    assert status_rank(effective_status({"status": "done"})) == 3
    assert status_rank(effective_status({"status": "skipped"})) == 4


def test_task_page_has_attention_sortable_table_dates_and_graph_tools(client):
    """Task page has a status-ranked sortable table with dates and graph tools."""
    res = client.get("/b/alpha/")
    assert res.status_code == 200
    assert "Needs attention" not in res.text
    assert "data-sort" in res.text
    assert "Activity" in res.text
    assert "graph-wrap" in res.text
    assert "htmx:afterSwap" in res.text


def test_slice_key_copies_and_running_steps_show_progress(tmp_path):
    """Keys carry a copy button; planned slices with running steps badge progress."""
    from pi_job_viewer.render import task_fragment
    from pi_job_viewer.store import BundleHandle, SliceSummary

    task = {
        "title": "T",
        "decisions": [],
        "plan": {
            "slices": [
                {
                    "key": "run",
                    "title": "Run",
                    "kind": "implement",
                    "status": "planned",
                    "goal": "",
                    "depends_on": [],
                    "steps": [{"key": "s", "title": "S", "status": "in_progress"}],
                    "final_steps": [],
                }
            ]
        },
    }
    handle = BundleHandle(
        slug="t",
        root=tmp_path,
        task=task,
        status="planned",
        slices=(SliceSummary(key="run", title="Run", kind="implement", status="planned", goal=""),),
    )
    html = task_fragment(handle, "", "flowchart LR")
    assert 'data-copy="run"' in html
    assert "pill-progress" in html
    assert 'title="stored: planned"' in html


def test_slice_steps_show_notes_column():
    """Slice detail renders each step note as Markdown in its own column."""
    from pi_job_viewer.render import slice_fragment

    sl = {
        "key": "k",
        "title": "K",
        "status": "in_progress",
        "goal": "",
        "steps": [{"key": "s", "title": "S", "status": "done", "note": "Did **things**."}],
        "final_steps": [],
    }
    html = slice_fragment("b", sl, "")
    assert "<th>Note</th>" in html
    assert "<strong>things</strong>" in html


def test_breadcrumb_trail_marks_tasks_slug_slices_slice(client):
    """Task and slice pages carry the tasks / slug / slices / key trail."""
    task = client.get("/b/alpha/")
    assert task.status_code == 200
    assert 'id="crumbs"' in task.text
    assert ">tasks<" in task.text
    assert ">alpha<" in task.text
    assert ">slices<" not in task.text
    sl = client.get("/b/alpha/slice/do-the-change")
    assert sl.status_code == 200
    assert ">tasks<" in sl.text
    assert ">slices<" in sl.text
    assert ">do-the-change<" in sl.text


def test_htmx_swap_carries_out_of_band_crumbs(client):
    """htmx responses update the header trail outside the swap target."""
    res = client.get("/partial/bundles/alpha/slices/do-the-change", headers={"hx-request": "true"})
    assert res.status_code == 200
    assert 'hx-swap-oob="true"' in res.text
    assert ">slices<" in res.text


def test_ordered_slices_rank_status_then_claim_recency():
    """Display order: status rank first, freshest claim heartbeat next."""
    from pi_job_viewer.store import ordered_slices

    task = {
        "orchestration": {
            "cursors": [
                {"owner": "a", "slice": "old", "claimed_at": "2026-09-20T00:00:00Z", "last_seen": "2026-09-20T00:00:00Z"},
                {"owner": "b", "slice": "new", "claimed_at": "2026-09-28T00:00:00Z", "last_seen": "2026-09-28T00:00:00Z"},
            ]
        },
        "plan": {
            "slices": [
                {"key": "done", "status": "done", "depends_on": []},
                {"key": "old", "status": "planned", "depends_on": []},
                {"key": "new", "status": "planned", "depends_on": []},
                {"key": "busy", "status": "in_progress", "depends_on": []},
            ]
        },
    }
    assert [s["key"] for s in ordered_slices(task)] == ["busy", "new", "old", "done"]


def test_slice_shows_worktree_and_pr_links_copyable():
    """Slice detail lists worktree paths and PR URLs, each with a copy button."""
    from pi_job_viewer.render import slice_fragment

    sl = {
        "key": "k",
        "title": "K",
        "status": "in_progress",
        "goal": "",
        "steps": [],
        "final_steps": [],
        "repo_work": {
            "graphius": {
                "worktree": "/tmp/wt/k/graphius",
                "prs": [{"url": "https://example.test/pr/1", "status": "open"}],
            }
        },
    }
    html = slice_fragment("b", sl, "")
    assert "<h2>Repo work</h2>" in html
    assert 'data-copy="/tmp/wt/k/graphius"' in html
    assert 'href="https://example.test/pr/1"' in html
    assert 'data-copy="https://example.test/pr/1"' in html


def test_slice_without_repo_work_omits_section():
    """Slices with no recorded repo work show no empty section."""
    from pi_job_viewer.render import slice_fragment

    html = slice_fragment("b", {"key": "k", "steps": [], "final_steps": []}, "")
    assert "Repo work" not in html


def test_task_page_lists_references(client):
    """Task page shows the references index plus bundle file links."""
    res = client.get("/b/alpha/")
    assert res.status_code == 200
    assert "<h2>References</h2>" in res.text
    assert "references/index.md" in res.text


def test_slice_page_lists_related_references(client, home):
    """Slice page links reference files mentioning the slice key."""
    (home / "alpha" / "references" / "do-the-change-notes.md").write_text(
        "Notes for do-the-change.\n", encoding="utf-8"
    )
    res = client.get("/b/alpha/slice/do-the-change")
    assert res.status_code == 200
    assert "<h2>Related references</h2>" in res.text
    assert "do-the-change-notes.md" in res.text


def test_partial_file_returns_fragment_for_htmx(client, home):
    """File partial swaps get the bare fragment with styled shell on direct visit."""
    frag = client.get("/partial/bundles/alpha/plans/do-the-change.md", headers={"hx-request": "true"})
    assert frag.status_code == 200
    assert "stylesheet" not in frag.text
    assert "Ship it." in frag.text
    direct = client.get("/partial/bundles/alpha/plans/do-the-change.md")
    assert direct.status_code == 200
    assert "stylesheet" in direct.text


def test_external_artifact_paths_show_absolute_with_copy(tmp_path):
    """Out-of-bundle artifact paths render absolute with a copy button, no link."""
    from pi_job_viewer.render import references_section
    from pi_job_viewer.store import BundleHandle

    handle = BundleHandle(
        slug="t",
        root=tmp_path,
        task={"decisions": []},
        status="planned",
        files=(("references/a.md", True), ("/tmp/out.md", False)),
    )
    html = references_section(handle)
    assert "outside bundle" in html
    assert 'data-copy="/tmp/out.md"' in html


def test_file_links_open_as_full_page(client):
    """Reference file links replace the main view with a pushed URL."""
    res = client.get("/b/alpha/")
    assert 'hx-target="#main"' in res.text
    assert "?stack=1" not in res.text


def test_file_partial_returns_full_page_fragment(client, home):
    """File swaps replace the main view and still render the body."""
    res = client.get(
        "/partial/bundles/alpha/plans/do-the-change.md", headers={"hx-request": "true"}
    )
    assert res.status_code == 200
    assert 'class="column"' not in res.text
    assert "data-close" not in res.text
    assert 'hx-swap-oob="true"' in res.text
    assert "Ship it." in res.text


def test_decisions_hide_in_collapsed_container(client, home):
    """Decisions collapse behind a summary with a count, expanded on demand."""
    env = dict(os.environ, PI_JOB_TASKS=str(home))
    _run_viewer_cli(env, "--task", "alpha", "add-decision", "--date", "2026-09-28", "--note", "Hidden gem", "--source", "chat:test")
    res = client.get("/b/alpha/")
    assert res.status_code == 200
    assert "<details" in res.text
    assert "Decisions (" in res.text
    assert "Hidden gem" in res.text


def test_frontmatter_links_beat_heuristic_and_unlinked_shows_gap(client, home):
    """Explicit slices frontmatter leads Related; unlinked files surface on task."""
    refs = home / "alpha" / "references"
    (refs / "linked.md").write_text("---\nslices: [do-the-change]\n---\nClaimed.\n", encoding="utf-8")
    (refs / "do-the-change-scratch.md").write_text("Scratch for do-the-change.\n", encoding="utf-8")
    (refs / "loose.md").write_text("Nothing linked here.\n", encoding="utf-8")
    sl = client.get("/b/alpha/slice/do-the-change")
    assert sl.status_code == 200
    assert sl.text.index("linked.md") < sl.text.index("do-the-change-scratch.md")
    task = client.get("/b/alpha/")
    assert task.status_code == 200
    assert "Unlinked references" in task.text
    assert "loose.md" in task.text


def test_no_stack_markup_or_styles_remain(client):
    """Skin and shell carry no side-column stack markup or styles."""
    res = client.get("/static/style.css")
    assert res.status_code == 200
    assert ".stack" not in res.text
    assert ".column" not in res.text
    page = client.get("/")
    assert 'id="stack"' not in page.text


def test_stats_page_renders_harness_stats_markdown(client):
    """Stats page shows the same Timeline and Status sections as pi-job stats."""
    res = client.get("/b/alpha/stats")
    assert res.status_code == 200
    assert "Timeline" in res.text
    assert "Status" in res.text
    assert ">stats<" in res.text


def test_partial_stats_returns_fragment_for_htmx(client):
    """Stats swaps get the bare fragment with styled shell on direct visit."""
    frag = client.get("/partial/bundles/alpha/stats", headers={"hx-request": "true"})
    assert frag.status_code == 200
    assert "stylesheet" not in frag.text
    assert "Timeline" in frag.text
