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

## Why no automatic stand/sit detection

Browsers expose motion sensors (`DeviceOrientationEvent`, Generic Sensor `Accelerometer`) only on hardware that
reports them — phones and some tablets. MacBooks have no accelerometer the browser can read, and a laptop moving up
with the desk doesn't change its orientation anyway. The keyboard shortcut is the reliable switch.

## Install

1. `brave://extensions` (or `chrome://extensions`) → enable *Developer mode*.
2. *Load unpacked* → select this folder.
3. Optional: pin the icon. The badge shows minutes to the next break.

## Test

```sh
node --test laps.test.mjs
```
