# Why `ssh-add` burns CPU under GNOME `gcr-ssh-agent`

A short post-mortem of stuck `/usr/bin/ssh-add` processes that sit at high CPU.

The symptom looks like a runaway `ssh-add` on your private key path.
The parent is not your shell.
GNOME `gcr-ssh-agent` starts those children when it tries to unlock a key for the wrapped `ssh-agent`.

Keep live host facts in the investigation chat.
This file uses placeholders only.

---

## Part 0: The 60-second version

GNOME ships a user service that wraps OpenSSH `ssh-agent`.
The wrapper watches keys under `~/.ssh/` and runs `ssh-add` when a client needs a passphrase-protected key.

`ssh-add` has no TTY in that path (`stdin` is `/dev/null`).
It uses `SSH_ASKPASS` (`gcr4-ssh-askpass`) and D-Bus `org.gnome.keyring.SystemPrompter`.

When the prompt process exits with an error, or when the keyring feeds a stale passphrase, `ssh-add` does not always exit.
It busy-loops.
Each later SSH or git call can spawn another copy.

The agent can already list the same key as loaded.
The spinning children are leftover unlock attempts, not extra useful work.

```
SSH / git client
        |
        v
SSH_AUTH_SOCK --> gcr-ssh-agent.socket
        |
        v
gcr-ssh-agent  -->  ssh-agent (real keys in memory)
        |
        +-- spawn ssh-add <private-key>
                stdin = /dev/null
                SSH_ASKPASS = gcr4-ssh-askpass
                        |
                        v
                SystemPrompter (D-Bus)
                        |
                        +-- success: passphrase -> ssh-add exits
                        +-- fail: ssh-add can spin; wrapper logs ChildExited
```

---

## Part 1: Post-mortem

### 1.1 Symptom

`top` or `htop` shows several `/usr/bin/ssh-add` lines on one `id_*` private key.
Each line uses a large share of one core.
Elapsed time grows for hours.
The processes do not finish.

### 1.2 First hypothesis

A login script or direnv loop keeps calling `ssh-add` from a shell.

That hypothesis is wrong when every `ssh-add` shares one parent: `gcr-ssh-agent`.

### 1.3 Live checks

Confirm the parent:

```bash
ps -o pid,ppid,args -C ssh-add
pstree -asp "$(pgrep -n gcr-ssh-agent)"
```

Confirm the user units:

```bash
systemctl --user status gcr-ssh-agent.service gcr-ssh-agent.socket
journalctl --user -u gcr-ssh-agent.service --since '1 day ago'
```

A typical failure line is:

```text
couldn't prompt for password: GDBus.Error:org.freedesktop.DBus.Error.Spawn.ChildExited: Process org.gnome.keyring.SystemPrompter exited with status 1
the /usr/bin/ssh-add command failed: Child process exited with code 1
```

Confirm `ssh-add` has no TTY:

```bash
ls -l /proc/<pid>/fd
```

Expect fd 0 as `/dev/null`.
Expect `SSH_ASKPASS` in `/proc/<pid>/environ` to name `gcr4-ssh-askpass`.

List keys already in the agent:

```bash
SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/gcr/ssh" ssh-add -l
```

A listed key does not prove the spinning children are useful.
They can be stuck unlock loops next to a key that already loaded.

### 1.4 Why CPU is high

`ssh-add` is in state `R` (running), not sleeping on a prompt.
`wchan` is empty (`0`).
That is a user-space busy loop, not a blocked `read` on a passphrase dialog.

Known matching reports:

- GNOME `gcr` work item on bad or stale keyring secrets for SSH keys
- Distro threads where `gcr-ssh-agent` keeps spawning `ssh-add` after `SystemPrompter` dies
- Cases where the keyring stores an empty or old passphrase after `ssh-keygen -p`

The wrapper retries unlock on later client use.
Failed prompts leave orphan `ssh-add` processes.

### 1.5 Immediate relief

Kill only the stuck `ssh-add` children.
Do not kill `ssh-agent` unless you also plan to reload keys.

```bash
pkill -f '/usr/bin/ssh-add'
```

If they return on the next `git fetch` or `ssh`:

1. Open Seahorse (Passwords and Keys).
2. Delete the stale "Unlock password for:" entry for that SSH key.
3. Unlock once from a real TTY: `ssh-add ~/.ssh/id_ed25519`.
4. Or stop using GCR as the agent for this session.

To stop the wrapper for this user:

```bash
systemctl --user disable --now gcr-ssh-agent.socket gcr-ssh-agent.service
```

Then point `SSH_AUTH_SOCK` at a normal `ssh-agent` and add the key from a terminal.

---

## Part 2: Pipeline stages

Map the failure before you change shell rc files.

```
[1] Client talks to SSH_AUTH_SOCK (gcr socket)
[2] gcr-ssh-agent decides the key needs unlock
[3] ssh-add runs with no TTY
[4] ASKPASS / SystemPrompter must collect the passphrase
[5] ssh-agent stores the unlocked key
```

High CPU at stage 3 with a journal error at stage 4 means the prompt path failed.
A `ssh-add` in `~/.bashrc` is a different pipeline.
Do not "fix" stage 1 shell config when the parent is `gcr-ssh-agent`.

---

## Part 3: What not to do

Do not paste private key paths, fingerprints, or host names into a public copy of this note.

Do not treat `ssh-add -l` success as proof the CPU processes are healthy.

Do not `kill` the whole `gcr-ssh-agent` tree as the first step if you still need the loaded keys.
Kill the spinning `ssh-add` PIDs first.
Then fix the keyring entry or the prompt service.
