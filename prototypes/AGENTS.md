# Prototype Instructions

This directory contains throwaway, one-file HTML prototypes published through GitHub Pages.

## Principles

- Prefer a single self-contained `.html` file per prototype.
- Put CSS in a `<style>` tag and JavaScript in a `<script>` tag inside the same file.
- Do not add a bundler, package manager, framework, build step, or dependency unless explicitly requested.
- Design mobile-first, then make the layout acceptable on desktop.
- Keep prototypes easy to open directly from the filesystem and from GitHub Pages.
- Use plain browser APIs before adding libraries.
- Keep state local to the page unless persistence is part of the prototype.
- Use `localStorage` only for user settings or session history that should survive reloads.
- Optimize for fast iteration, clear interaction, and realistic feel over production architecture.
- Make primary actions large, touch-friendly, and visible without scrolling on mobile.
- Keep controls understandable without external documentation.
- Include enough inline labels and stats for the prototype to explain itself while being used.
- Avoid backend services, authentication, analytics, and network calls unless they are the thing being prototyped.
- Keep filenames URL-friendly, lowercase, and hyphen-separated.
- If there are multiple prototypes, maintain `index.html` as a simple launcher page.

## GitHub Pages Compatibility

- Assume prototypes are served as static files from the repository root.
- Use relative links between files.
- Avoid absolute paths unless they intentionally include the repository name.
- Do not rely on server-side routing, rewrites, environment variables, or generated assets.
