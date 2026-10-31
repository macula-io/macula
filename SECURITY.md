# Security policy

## Reporting a vulnerability

Email **security@macula.io**. Please do not open a public issue for anything
that could be exploited against the running Macula fleet (the stations, the
realm or the services on it).

Tell us, as far as you can:

- the repository, and the release or commit you looked at;
- what you found and how to reproduce it;
- what an attacker could do with it.

Plain email is fine; there is no encryption key to use.

## How we handle a report

- **Exploitable on the running fleet:** kept private until it is fixed, then
  published as an advisory.
- **Hardening findings** (a weakness nothing can exploit today): filed as a
  public issue, written in defensive terms: the invariant, the limit, the fix
  and the test.

## What to expect

We confirm that we received your report within 3 working days, and tell you
which of the two it is within 10 working days. After that we keep you
informed until it is closed. One person reads this address, so an absence
can delay us; if you hear nothing within those times, please send it again.
We credit you in the advisory or the issue unless you ask us not to.

Reports are read by a person. No AI tool, connector or summariser reads
this address.

## Attachments, patches and identity

- Put the details in the body of the email. We never open an attachment on
  a machine that signs our releases.
- A patch you offer is reviewed like any other change before we use it.
- Saying who you are does not give you access to any report. We reply only
  on the thread you started, and only with receipt, classification and
  status.

## Drills

Now and then we send ourselves a test report to check that this path
works. Its subject starts with DRILL and a one-time code we record
beforehand. A message marked DRILL with any other code is handled as a
real report.

## Scope

This policy covers this repository and the rest of Macula: the SDKs, the
relay stations, the realm, the mcl-* services and the fleet they run on.
The fleet is a development fleet; please ask before running load or
denial-of-service tests against it. A vulnerability in a third-party
dependency belongs with its maintainers; tell us too if it affects Macula.
