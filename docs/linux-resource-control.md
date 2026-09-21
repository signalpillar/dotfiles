# Keep a Linux desktop responsive under heavy Node jobs

A short post-mortem of a freeze where a Node process filled RAM and swap.
The dest files live in this chezmoi tree.
Live host sizes stay out of this page.

---

## Part 0: The 60-second version

The kernel does not protect the compositor by default.
A user process can allocate until RAM fills.
Then the machine pages to disk swap and the UI stalls.

Control groups (cgroups) put a wall around a process tree.
systemd names those trees as slices and units.
`systemd-oomd` kills a tree that causes memory pressure, if you enable the watch.

This repo:

1. Caps every user slice so the kernel keeps a RAM reserve.
2. Turns on swap-based oomd kills on the root slice.
3. Adds compressed RAM swap (zram) so oomd has time to act.
4. Caps `background.slice` for builds and dev servers.
5. Protects `session.slice` from oomd.
6. Wraps Node jobs with `run-limited`.

Start a heavy job with:

```bash
run-limited npm run dev
```

---

## Part 1: Post-mortem of what failed

### Symptom

The desktop froze.
A Node process used most of the CPU and RAM.

That symptom hides the cause.
The compositor still ran.
It waited on memory reclaim.

### Baseline vs this config

Ubuntu already runs `systemd-oomd`.
The vendor drop-in sets `ManagedOOMMemoryPressure=kill` on `user@.service`.
That watch waits on pressure for tens of seconds.

The baseline on this class of machine also had:

- `MemoryMax=infinity` on the user slice
- no swap-monitored cgroups (`ManagedOOMSwap` stayed `auto`)
- a disk swap file that filled
- Node in the default session scope, next to the desktop

Pressure watch without a memory wall lets swap fill first.
Disk swap at the limit is thrash.
Thrash looks like a freeze.
oomd then either waits, or kills a large user cgroup.

### Failing pipeline stage

Map the stall to one stage.
Do not treat "Node is busy" as the diagnosis.

```
[1] process allocates heap / RSS
[2] cgroup MemoryHigh / MemoryMax
[3] swap (zram, then disk)
[4] systemd-oomd (pressure or swap)
[5] kernel OOM killer
[6] compositor and TTY stay scheduled
```

The baseline failed at stage 2 (no wall) and stage 4 (swap watch off).
Stage 3 then used a slow disk swap file until the UI stalled.
Stage 5 did not save the session in time.

### Decision rule

If the UI stalls while a job allocates:

1. Read live units, not only grep of a conf file.
2. Confirm which slice owns the job (`/proc/PID/cgroup`).
3. Name the first missing wall: memory cap, swap watch, or job isolation.
4. Fix that stage.
5. Re-read the same properties.

Commands that show live state:

```bash
systemctl show "user-$(id -u).slice" -p MemoryHigh -p MemoryMax
systemctl show -- -.slice -p ManagedOOMSwap
systemctl --user show background.slice -p MemoryHigh -p MemoryMax
systemctl --user show session.slice -p MemoryMin -p ManagedOOMPreference
oomctl
swapon --show
```

Use `systemctl show -- -.slice` for the root slice.
A bare `-.slice` looks like a flag.

After a user drop-in change, run `systemctl --user daemon-reload`.
After an `/etc` drop-in change, run `sudo systemctl daemon-reload`.

---

## Part 2: Concepts and how they relate

### cgroup

A cgroup is a kernel group of processes.
The kernel accounts CPU, memory, and I/O per group.
A limit on the group applies to every member.

systemd creates the groups.
You set limits on systemd units.

See `systemd.resource-control(5)`.

### Slice

A slice is a unit that only groups other units.
systemd defines three user slices for a graphical session:

| Slice | Role |
| --- | --- |
| `session.slice` | compositor, session buses |
| `app.slice` | normal apps |
| `background.slice` | low-priority jobs |

See `systemd.special(7)` and the systemd desktop notes:
https://github.com/systemd/systemd/blob/main/docs/DESKTOP_ENVIRONMENTS.md

`background.slice` already has `CPUWeight=30`.
Session and app slices default to `CPUWeight=100`.
Weight is a share, not a hard cap.

### MemoryHigh vs MemoryMax

`MemoryHigh=` throttles the group when it crosses the line.
`MemoryMax=` is the last wall.
If the group cannot stay under `MemoryMax`, the cgroup OOM killer runs inside that group.

systemd recommends `MemoryHigh` as the main control and `MemoryMax` as the last line.
Percent values are of physical RAM.

This tree sets 80% / 90% on every `user-.slice`:

```1:7:linux/resource-control/user-slice-memory.conf
# Applied to /etc/systemd/system/user-.slice.d/50-memory.conf
# Caps every user slice so kernel and system.slice keep a RAM reserve.
# Percentages are of physical memory. See systemd.resource-control(5).
[Slice]
MemoryAccounting=yes
MemoryHigh=80%
MemoryMax=90%
```

Background jobs get a tighter wall:

```1:8:dot_config/systemd/user/background.slice.d/50-limits.conf
# Hard wall for low-priority user jobs (dev servers, builds).
# systemd puts those units in background.slice. CPUWeight=30 is the default.
# Percentages are of physical memory. See systemd.resource-control(5).
[Slice]
MemoryAccounting=yes
MemoryHigh=35%
MemoryMax=45%
ManagedOOMMemoryPressure=kill
```

### CPUWeight vs CPUQuota

`CPUWeight=` changes relative share when the CPUs are busy.
`CPUQuota=` is a hard cap.
`100%` means one full CPU.

Do not put a tight `CPUQuota` on the whole user slice.
That caps the compositor too.

`run-limited` sets `CPUQuota` to 70% times `nproc` on one job.

### systemd-oomd

`systemd-oomd` is a userspace killer.
It reads PSI (pressure stall information) and optional swap usage.
It kills a **descendant** cgroup of a unit with `ManagedOOM*=kill`.
The unit that sets `kill` is not itself a candidate.

Vendor Ubuntu already sets pressure kill on `user@.service`.
Swap kill stays off until an ancestor sets `ManagedOOMSwap=kill`.

This tree sets that on the root slice:

```1:5:linux/resource-control/root-slice-oomd-swap.conf
# Applied to /etc/systemd/system/-.slice.d/50-oomd-swap.conf
# Lets systemd-oomd kill descendant cgroups when system swap is exhausted.
# Swap monitoring is off unless an ancestor sets ManagedOOMSwap=kill.
[Slice]
ManagedOOMSwap=kill
```

oomd needs some swap.
With no swap, the machine reaches a livelock faster than oomd can act.
See `systemd-oomd.service(8)`.

`ManagedOOMPreference=omit` on `session.slice` keeps the session off the kill list:

```1:6:dot_config/systemd/user/session.slice.d/50-protect.conf
# Keep the graphical session off the oomd candidate list.
# session.slice holds the compositor and session buses.
# See systemd.special(7) and systemd.resource-control(5) ManagedOOMPreference.
[Slice]
MemoryMin=256M
ManagedOOMPreference=omit
```

### zram vs disk swap

Disk swap at the limit causes thrash.
zram is swap in compressed RAM.
It is slower than free RAM and faster than a disk swap file.
Give it a higher swap priority than the disk file.

```1:7:linux/resource-control/zram-generator.conf
# Applied to /etc/systemd/zram-generator.conf
# Compressed RAM swap with higher priority than a disk swap file.
# Gives systemd-oomd time to act before the machine thrashes on disk.
# Keys: https://github.com/systemd/zram-generator
[zram0]
zram-size = min(ram / 4, 4096)
compression-algorithm = zstd
```

Do not treat swap as extra RAM for Node.
Treat swap as a buffer so oomd can fire.

### V8 heap vs RSS

`--max-old-space-size=4096` caps the V8 old-space heap in MiB.
RSS is still larger (stacks, buffers, native addons).
Use the flag **and** the cgroup `MemoryMax`.

`run-limited` sets the flag for `node`, `npm`, `npx`, `pnpm`, `yarn`, and `bun` only.

### earlyoom

`earlyoom` kills by free RAM or swap percent.
It is faster and dumber than oomd.
This tree does not install it.
Two userspace killers on one box fight over the same victim.

---

## Part 3: Pipeline diagram

```
                    physical RAM
                         |
         +---------------+---------------+
         |                               |
   system.slice                    user-.slice
   (kernel helpers,                MemoryHigh=80%
    docker)                        MemoryMax=90%
                                         |
                    +--------------------+--------------------+
                    |                    |                    |
             session.slice          app.slice         background.slice
             MemoryMin=256M         default           MemoryHigh=35%
             oomd omit                                MemoryMax=45%
                    |                                     |
             compositor                              run-limited job
                                                      CPUQuota=70%*nproc
                                                      NODE_OPTIONS heap cap

   swap:  zram (high prio)  then  disk file (low prio)
   oomd:  pressure on user@.service  +  swap kill from -.slice
```

A Node job started without `run-limited` stays in the default scope.
It still sits under `user-.slice`, so the 90% wall still applies.
It does **not** get the 45% background wall or the CPU quota.

---

## Part 4: What chezmoi owns

| Source | Destination |
| --- | --- |
| `linux/resource-control/user-slice-memory.conf` | `/etc/systemd/system/user-.slice.d/50-memory.conf` |
| `linux/resource-control/root-slice-oomd-swap.conf` | `/etc/systemd/system/-.slice.d/50-oomd-swap.conf` |
| `linux/resource-control/zram-generator.conf` | `/etc/systemd/zram-generator.conf` |
| `dot_config/systemd/user/background.slice.d/50-limits.conf` | `~/.config/systemd/user/background.slice.d/50-limits.conf` |
| `dot_config/systemd/user/session.slice.d/50-protect.conf` | `~/.config/systemd/user/session.slice.d/50-protect.conf` |
| `dot_local/bin/executable_run-limited` | `~/.local/bin/run-limited` |

`linux/` is in `.chezmoiignore`.
`include()` still reads those payloads from source.
[run_onchange_linux-resource-control.sh.tmpl](run_onchange_linux-resource-control.sh.tmpl) copies them with `sudo` on Linux.

User files apply with `chezmoi apply`.
Then run `systemctl --user daemon-reload`.

System files need a sudo session.
On Linux, run `chezmoi apply` after `sudo -v` so the on-change script can install packages and drop-ins.

Do not add a global `NODE_OPTIONS` in the shell rc.
A global heap cap breaks tools that are not your dev server.

---

## Part 5: How this maps to config-investigation

The config-investigation skill treats a symptom as a pipeline, not as a file to grep.

Same method here:

| Skill step | This problem |
| --- | --- |
| Reproduce in the real environment | Freeze under a real Node job, not a unit-test |
| Minimal baseline | Vendor systemd: oomd on, `MemoryMax=infinity`, no swap watch |
| Read live state | `systemctl show`, `oomctl`, `swapon`, `/proc/PID/cgroup` |
| Confirm which file loads | User drop-ins under `~/.config/systemd/user/` vs `/etc` drop-ins from the on-change script |
| Name the failing stage | Stage 2 (no memory wall) and stage 4 (swap watch off) |
| Smallest correct change | Walls on slices + zram + `run-limited`, not a new OOM daemon |
| Re-test with evidence | Same `systemctl show` / `oomctl` pair as the baseline |

Grep of a conf file shows intent.
`systemctl show` shows what systemd loaded.
If the property is still `infinity` after apply, the drop-in did not load.
Check the path and `daemon-reload`.

---

## Glossary

| Term | Meaning |
| --- | --- |
| cgroup | Kernel group that accounts and limits resources for a process tree |
| slice | systemd unit that only groups other units |
| scope | Transient cgroup for a process started with `systemd-run --scope` |
| `MemoryHigh` | Throttle line. The group pays reclaim cost above this |
| `MemoryMax` | Hard wall. Crossing it invokes the cgroup OOM killer |
| `CPUWeight` | Relative CPU share when the machine is busy |
| `CPUQuota` | Hard CPU cap. `100%` is one CPU |
| PSI | Pressure stall information. Share of time tasks wait on memory or I/O |
| systemd-oomd | Userspace killer that uses PSI and optional swap limits |
| `ManagedOOMSwap=kill` | Ask oomd to watch descendants for swap exhaustion |
| `ManagedOOMPreference=omit` | Keep this unit off the oomd candidate list |
| zram | Block device that stores swap pages compressed in RAM |
| RSS | Resident set size. Pages in RAM, not only the JS heap |
| V8 old-space | Long-lived JS heap. `--max-old-space-size` caps this in MiB |

---

## Sources

- `systemd.resource-control(5)`: https://www.freedesktop.org/software/systemd/man/latest/systemd.resource-control.html
- `systemd-oomd.service(8)`: https://www.freedesktop.org/software/systemd/man/latest/systemd-oomd.service.html
- `oomd.conf(5)`: https://www.freedesktop.org/software/systemd/man/latest/oomd.conf.html
- systemd desktop slices: https://github.com/systemd/systemd/blob/main/docs/DESKTOP_ENVIRONMENTS.md
- zram-generator: https://github.com/systemd/zram-generator
