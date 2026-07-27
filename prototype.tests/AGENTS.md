# Prototype Test Instructions

This directory holds reusable tests for the one-file apps in `prototypes/`.

- Keep each prototype's tests under `<prototype-name>/tests`.
- Keep test dependencies and downloaded fixtures outside `prototypes/` so the HTML apps remain build-free.
- Validate the embedded JSON manifest before launching a browser.
- Exercise both the normal dependency path and the CDN-blocked fallback path.
- Route CDN requests to local fixture copies rather than relying on live network access during browser tests.
- Do not commit generated reports, browser downloads, `node_modules`, or fetched third-party fixtures.
