# Standing Desk Coach

MV3 extension (Brave / Chrome) for mindful, hands-free movement breaks while you stand.

- **Toggle** standing/sitting in the popup or with `Alt+Shift+S`. Standing starts the break timer; sitting stops it.
- **Every N minutes** (20 / 30 / 45 / 60, default 30) you get a notification — or the session window opens directly.
- **Session**: box-breathing arrival → 2, 3 or all 6 exercises (rotating each break) → a short return-to-focus.
  Every rep, hold and side switch is a timed lap with voice + chime cues, so after pressing *Begin* it is hands-free.
- Each exercise shows the article's illustration, full instructions, and links to the article and its medical source.
- **Sitting** (toggle off): a reminder to stand up after 30 / 45 / 50 / 60 min (default 50, or Off). "I'm standing" on the
  notification switches modes.
- Locking the screen pauses the timers; unlocking restarts the full interval.

Exercises and illustrations: [Work From Home Desks NZ — 6 Exercises You Can Do At Your Standing Desk](https://www.workfromhomedesks.co.nz/blogs/blog/standing-desk-exercises).

## Why short breaks

Loprinzi & Cardinal (Am J Health Promot, 2013; NHANES, n = 6,321, accelerometry) found activity accumulated in
bouts under 10 minutes was associated with health markers as strongly as ≥ 10-minute bouts. Cross-sectional, so an
association, not proof — but it supports many small breaks over one long session.
[doi:10.4278/ajhp.110916-QUAN-348](https://journals.sagepub.com/doi/abs/10.4278/ajhp.110916-QUAN-348)

## Why no automatic stand/sit detection

Browsers expose motion sensors (`DeviceOrientationEvent`, Generic Sensor `Accelerometer`) only on hardware that
reports them — phones and some tablets. MacBooks have no accelerometer the browser can read, and a laptop moving up
with the desk doesn't change its orientation anyway. The keyboard shortcut is the reliable switch.

## Install

1. `brave://extensions` (or `chrome://extensions`) → enable *Developer mode*.
2. *Load unpacked* → select this folder.
3. Optional: pin the icon. The badge shows minutes to the next break.

## Icon

`icon.svg` is the source. Regenerate the PNGs with:

```sh
for n in 16 32 48 128; do rsvg-convert -w $n -h $n icon.svg -o icons/icon$n.png; done
```

## Test

```sh
node --test laps.test.mjs
```
