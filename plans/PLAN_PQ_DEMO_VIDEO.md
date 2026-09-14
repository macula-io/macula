# Plan: post-quantum demonstration video

**This exists so the post-quantum cutover ships with a demonstration
video whose every claim is provable on the wire, doubling as the
partner-offer artifact the PQ plan's end goal already names.**

**Status:** Planning — no shots recorded yet. The recording can only
start once Stages 3-4 of `PLAN_POST_QUANTUM_SECURITY.md` stand up the
fleet and the stacks; this plan is the shot list and the claim
boundaries, ready for that day.
**Created:** 2026-09-14
**Classification:** BUILD — no claim to gate; the video IS the gate's
evidence, not an argument about the world.

## Why this demo is different

Most security demos assert what cannot be seen. Post-quantum work is
the rare exception: the proof is visible on the wire. The negotiated
key-share group appears in the ClientHello of every handshake, and a
classical-only client is *refused* by the new fleet — both are
capturable, checkable, and require no trust in the narrator. Stage 2
of the PQ plan ("prove it on the wire") already produces the raw
material; this plan packages it.

## Claim boundaries (the video must NOT say)

- Not "unbreakable": say "a quantum attacker must break BOTH halves
  of the hybrid; classical-only is refused everywhere."
- Not "done": signatures follow a published migration plan (D4, the
  10-year RSA realm CA moves first); the video shows key exchange
  live and the signature plan as a roadmap frame, not as shipped.
- Not "every stack": .NET is out of the first switch (D10) — the
  montage names the five stacks that dial, and says .NET follows.
- Every capture is a real dial against the real fleet; no scripted
  byte forgeries.

## Shot list

### 1. The handshake, before and after (the core shot)

- Capture: `tcpdump -i any -w pq-dial.pcap 'udp port 4433'` on the
  dialing host during one client dial against the PQ fleet; repeat
  against a pinned old station for the "before".
- Show: Wireshark filter `tls.handshake.extensions_key_share` —
  `X25519` in the old capture, `SecP384r1MLKEM1024` (EU profile) /
  `ML-KEM-1024` (US profile) in the new one.
- Materials: two pcaps + one filter. No editing tricks.

### 2. The refusal

- Two terminal panes: a pinned pre-cutover client fails to connect to
  the new fleet (the plan's success criterion: "refuses a
  classical-only client and X25519MLKEM768, in both profiles"); the
  current client connects on the first dial.
- The refusal is the "no classical-only mode anywhere" claim, proven
  instead of asserted.

### 3. Harvest-now, closed

- Same pcaps, split screen: the old capture is "one break away from
  plaintext"; the new one is hybrid. Narrated, not drawn.
- The old pcap IS the prop — captured from the real fleet before
  switch-off (D14 keeps the old fleet running alongside, so both
  captures can be made on the same day).

### 4. Six stacks, one handshake (the montage)

- Go, Rust, Python, TypeScript, PHP clients each dial the PQ fleet
  and print the negotiated group; five tiles fill in with "green
  wire checks". This is Stage 4's acceptance criterion, filmed.
- Each stack's one-liner is its own connect-and-report command
  (macula-cli `connect --json` for Go; the per-stack equivalents the
  Stage-4 suites already exercise).

### 5. The desktop, unchanged

- macula-desktop's Mesh tab: "connected" against the PQ fleet, with
  the capture overlay proving the group it is actually using. The
  user story: the strongest crypto upgrade the platform ever shipped,
  and nothing changed in the UI.

### 6. Two profiles, one mesh

- The same client dialing an EU-profile station (hybrid) and a
  US-profile station (pure ML-KEM) back to back — the
  jurisdiction-as-crypto-policy design (D1) made visible, with the
  realm names on screen.

## Materials checklist

- [ ] `tcpdump`/Wireshark captures: old-fleet dial, EU dial, US dial
- [ ] One pinned pre-cutover client binary for the refusal shot
- [ ] Five stacks' connect-and-report commands (from Stage-4 suites)
- [ ] macula-desktop build dialing the PQ fleet
- [ ] The signature-migration roadmap frame (one static slide, from
      `PLAN_POST_QUANTUM_SECURITY.md` §stages)

## Timing

Recording starts the day Stages 3-4 stand up (fleet + stacks), and
takes roughly one working day with the checklist above — most shots
are byproducts of work the team is doing anyway.

## Success criteria

- [ ] Every security claim in the video maps to a capture or a
      refusal the viewer could reproduce.
- [ ] The five-stack montage names .NET as following, not missing.
- [ ] The signature story is a roadmap frame, not a "done".
- [ ] Filename, date and station for every capture are on screen.
