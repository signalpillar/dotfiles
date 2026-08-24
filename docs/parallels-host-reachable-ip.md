# Find the Parallels Linux IP the Mac host can call

A short guide for a Linux guest under Parallels Desktop on a Mac.

The question: which address does the Mac host use to reach the guest?

Do not memorize one lease.
Shared-network addresses come from DHCP and can change after a restart.
Use the method below each time you need the address.

---

## Part 0: The 60-second version

A Linux guest often has many IPv4 addresses.
Docker owns most of them.
The Mac cannot use those.

The host-reachable address is the IPv4 on the default route.
On a Parallels **Shared** network that address sits in `10.211.55.0/24`.

```
Mac host
   |
   |  10.211.55.0/24  (Parallels Shared)
   v
guest NIC     -->  10.211.55.N     <-- call this
guest docker0 -->  172.17.0.1      ignore
guest br-*    -->  172.x.0.1       ignore
```

`N` is the guest host octet from DHCP.
It is not a fixed number.

Run this on the guest:

```bash
ip -4 route get 1.1.1.1
```

Read the `src` field.
That is the address the host can call.

---

## Part 1: Why `hostname -I` misleads

`hostname -I` prints every address the kernel knows.

That list often mixes the shared-net address, Docker bridges, and IPv6.

The Mac has no route into those Docker bridges.
If you pick `172.17.0.1` or another `172.x` bridge, the call stays inside the guest or fails on the host.

Treat `hostname -I` as a dump, not as an answer.

---

## Part 2: How to find the address

Run these on the Linux guest.

### 2.1 Fast path

```bash
ip -4 route get 1.1.1.1
```

Sample Shared-network output:

```text
1.1.1.1 via 10.211.55.1 dev enp0s5 src 10.211.55.N
```

| Field | Meaning |
| --- | --- |
| `via 10.211.55.1` | Parallels virtual router (DHCP/NAT gateway) |
| `dev enp0s5` | NIC that faces the host (the name can differ) |
| `src 10.211.55.N` | Address to call from the Mac |

The interface name can change.
The `src` field is the signal that matters.

### 2.2 Confirm the NIC

```bash
ip -4 route show default
ip -4 -br addr show scope global
```

Keep the address whose device matches the default route.
Drop any row whose device is `docker0` or `br-*`, or whose state is `DOWN`.

A typical keep row looks like this:

```text
enp0s5           UP             10.211.55.N/24
```

### 2.3 Narrow to one device

If you already know the NIC name from section 2.1:

```bash
ip -4 -br addr show dev enp0s5
```

Replace `enp0s5` with the `dev` you saw.
If you do not know the name, do not guess.
Use section 2.1 first.

### 2.4 Check from the Mac

On the Mac, ping the `src` address:

```bash
ping -c 2 10.211.55.N
```

Replace `N` with the octet from `src`.
A reply means the host can reach that guest address.
No reply means you picked a guest-local address, or Shared networking is off.

Parallels Desktop can also show the guest IP in the VM window or in `prlctl list -i`.
Verify those tools on the Mac when you need them.

---

## Part 3: What the other addresses are

| Address | Owner | Host can call it? |
| --- | --- | --- |
| `10.211.55.N` | Guest NIC on Parallels Shared | Yes |
| `10.211.55.1` | Gateway on the shared net | That is not the guest |
| Other `10.211.55.x` | Host or another VM on the shared net | Not this guest |
| `127.0.0.1` | Loopback | No |
| `172.17.0.1` | Typical `docker0` | No |
| Other `172.x.0.1` | Docker `br-*` | No |
| `fe80::...` | Link-local IPv6 | Not the usual host path |
| Unique-local IPv6 (`fd00::/8`) | Shared-net IPv6, if present | Possible, but IPv4 is the usual path |

Do not use the gateway or another neighbor as "the VM IP".

---

## Part 4: Parallels network modes

The prefix tells you which mode the VM uses.

| Guest IPv4 prefix | Mode | Who can call the guest |
| --- | --- | --- |
| `10.211.55.0/24` | Shared (usual default) | The Mac host. LAN peers usually cannot. |
| `10.37.129.0/24` | Host-only | The Mac host. No general internet path. |
| Same subnet as the Mac Wi-Fi or Ethernet | Bridged | The Mac and other LAN hosts. |

Shared uses a default route of the form `via 10.211.55.1 proto dhcp`.

If you switch the VM to Bridged, section 2.1 still works.
The `src` address then looks like a home or office LAN address, not `10.211.55.x`.

---

## Part 5: Decision rule

When several IPv4 addresses appear, pick one with this order:

1. Take the `src` from `ip -4 route get 1.1.1.1`.
2. Reject `127.0.0.1`.
3. Reject `docker0` and `br-*`.
4. On Shared, expect `10.211.55.x`.
5. Confirm with `ping` from the Mac.

The lease can change after a restart.
Run step 1 again.
Do not reuse yesterday's number without a check.

---

## Part 6: Terms used here

**Shared network.** Parallels NAT network between the Mac and the guest.
Typical prefix: `10.211.55.0/24`.

**Host-reachable address.** The guest IPv4 on that shared (or bridged) NIC.
The Mac uses it to open SSH, HTTP, or other ports you expose.

**Default route.** The path the guest uses for the internet.
Its `src` is the address that faces the host on Shared or Bridged.

**Docker bridge.** A network that exists only inside the guest.
The Mac has no route to it.
