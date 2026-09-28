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

We confirm that we received your report, tell you which of the two it is,
and keep you informed until it is closed. We credit you in the advisory or
the issue unless you ask us not to.

## Scope

This policy covers this repository and the rest of Macula: the SDKs, the
relay stations, the realm, the mcl-* services and the fleet they run on.
The fleet is a development fleet; please ask before running load or
denial-of-service tests against it. A vulnerability in a third-party
dependency belongs with its maintainers; tell us too if it affects Macula.
