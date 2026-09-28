"""FastAPI routes for the viewer (GET only, read-only).

Thin wiring over store.py reads and render.py fragments.
Every route returns text/html for htmx swaps.
Unknown slugs, slices, and files are 404; unreadable config is 500.
"""

from __future__ import annotations

import logging
from pathlib import Path

from fastapi import FastAPI, HTTPException, Request
from fastapi.responses import FileResponse, HTMLResponse, Response

from pi_job_viewer import render, store

log = logging.getLogger(__name__)
STATIC_DIR = Path(__file__).resolve().parent / "static"
MAX_FILE_BYTES = 2 * 1024 * 1024  # refuse giant bundle files instead of loading them whole


def _read_capped(path: Path) -> str:
    """Read a bundle file up to MAX_FILE_BYTES, else HTTP 413.

    Caps one unbounded read so a gigabyte plan cannot exhaust worker memory.
    """
    if path.stat().st_size > MAX_FILE_BYTES:
        log.warning("viewer-file-too-large", extra={"path": str(path)})
        raise HTTPException(status_code=413, detail=f"file too large: {path.name!r}")
    return path.read_text(encoding="utf-8", errors="replace")


def create_app(home: Path | None = None, *, only_slug: str | None = None) -> FastAPI:
    """Build the viewer app for one tasks home, optionally one pinned slug."""
    app = FastAPI(title="pi-job viewer")

    def bundles() -> list:
        """Bundle list, narrowed to the pinned slug in single-bundle mode."""
        found = store.list_bundles(home)
        if only_slug is not None:
            found = [b for b in found if b.slug == only_slug]
        return found

    @app.get("/static/style.css")
    def style() -> FileResponse:
        """Adwaita-style skin (the only static asset)."""
        return FileResponse(STATIC_DIR / "style.css", media_type="text/css")

    @app.get("/favicon.ico")
    def favicon() -> Response:
        """No icon ships; answer 204 so logs stay clean."""
        return Response(status_code=204)

    def _respond(request: Request, title: str, fragment: str, crumbs: str = "") -> HTMLResponse:
        """Fragment for htmx swaps, full shell for direct browser visits.

        A pasted or reloaded /partial/... URL must look identical to its
        canonical page, so non-htmx requests get the shell too. The header
        bar sits outside the swap target, so htmx responses carry the trail
        as an out-of-band swap that keeps it in sync on every navigation.
        """
        if request.headers.get("hx-request") == "true":
            oob = f"<nav class=\"crumbs\" id=\"crumbs\" hx-swap-oob=\"true\">{crumbs}</nav>"
            return HTMLResponse(oob + fragment)
        return HTMLResponse(render.shell(title, fragment, active=crumbs))

    @app.get("/")
    def index(request: Request) -> HTMLResponse:
        """Full page: bundle list."""
        crumbs = render.crumb_trail(("tasks", None, None))
        return _respond(request, "Bundles", render.bundle_list_fragment(bundles()), crumbs)

    @app.get("/partial/bundles")
    def partial_bundles(request: Request) -> HTMLResponse:
        """Bundle list: fragment for swaps, shell for direct visits."""
        crumbs = render.crumb_trail(("tasks", None, None))
        return _respond(request, "Bundles", render.bundle_list_fragment(bundles()), crumbs)

    def _task_page(slug: str) -> str:
        if only_slug is not None and slug != only_slug:
            raise HTTPException(status_code=404, detail=f"unknown bundle: {slug!r}")
        try:
            handle = store.open_bundle(slug, home)
        except store.BundleNotFound as exc:
            raise HTTPException(status_code=404, detail=str(exc)) from exc
        return render.task_fragment(
            handle, render.decisions_table(store.decision_records(handle)), store.dependency_graph(handle)
        )

    def _task_crumbs(slug: str) -> str:
        """Trail tasks / <slug> with a link back to the list."""
        return render.crumb_trail(
            ("tasks", "/", "/partial/bundles"),
            (slug, None, None),
        )

    def _slice_crumbs(slug: str, key: str) -> str:
        """Trail tasks / <slug> / slices / <key> with links up the tree."""
        return render.crumb_trail(
            ("tasks", "/", "/partial/bundles"),
            (slug, f"/b/{slug}/", f"/partial/bundles/{slug}"),
            ("slices", None, None),
            (key, None, None),
        )

    def _file_crumbs(slug: str, kind: str, name: str) -> str:
        """Trail tasks / <slug> / <kind> / <name> with links up the tree."""
        return render.crumb_trail(
            ("tasks", "/", "/partial/bundles"),
            (slug, f"/b/{slug}/", f"/partial/bundles/{slug}"),
            (kind, None, None),
            (name, None, None),
        )

    @app.get("/b/{slug}/")
    def task_page(slug: str, request: Request) -> HTMLResponse:
        """Full page: task overview for one bundle."""
        return _respond(request, slug, _task_page(slug), _task_crumbs(slug))

    @app.get("/partial/bundles/{slug}")
    def partial_task(slug: str, request: Request) -> HTMLResponse:
        """Task overview: fragment for swaps, shell for direct visits."""
        return _respond(request, slug, _task_page(slug), _task_crumbs(slug))

    def _slice_page(slug: str, key: str) -> str:
        try:
            handle = store.open_bundle(slug, home)
            sl = store.slice_detail(handle, key)
        except store.BundleNotFound as exc:
            raise HTTPException(status_code=404, detail=str(exc)) from exc
        try:
            plan_text = _read_capped(store.bundle_file(handle, "plans", f"{key}.md"))
            plan_html = render.render_markdown(plan_text)
        except store.BundleNotFound:
            plan_html = "<p><em>No plan file.</em></p>"
        return render.slice_fragment(slug, sl, plan_html)

    @app.get("/b/{slug}/slice/{key}")
    def slice_page(slug: str, key: str, request: Request) -> HTMLResponse:
        """Full page: one slice plus its rendered plan file."""
        return _respond(request, f"{slug} / {key}", _slice_page(slug, key), _slice_crumbs(slug, key))

    @app.get("/partial/bundles/{slug}/slices/{key}")
    def partial_slice(slug: str, key: str, request: Request) -> HTMLResponse:
        """Slice detail: fragment for swaps, shell for direct visits."""
        return _respond(request, f"{slug} / {key}", _slice_page(slug, key), _slice_crumbs(slug, key))

    def _file_page(slug: str, kind: str, name: str) -> str:
        if kind not in ("plans", "references"):
            raise HTTPException(status_code=404, detail=f"unknown section: {kind!r}")
        try:
            handle = store.open_bundle(slug, home)
            path = store.bundle_file(handle, kind, name)
        except store.BundleNotFound as exc:
            raise HTTPException(status_code=404, detail=str(exc)) from exc
        text = _read_capped(path)
        if path.suffix.lower() == ".md":
            return render.file_fragment(f"{slug} / {kind}/{name}", render.render_markdown(text))
        return render.file_fragment(f"{slug} / {kind}/{name}", f"<pre>{render.esc(text)}</pre>")

    @app.get("/b/{slug}/{kind}/{name:path}")
    def file_page(slug: str, kind: str, name: str, request: Request) -> HTMLResponse:
        """Full page: one plans/ or references/ file, Markdown rendered when possible."""
        return _respond(request, f"{slug} / {name}", _file_page(slug, kind, name), _file_crumbs(slug, kind, name))

    return app
