# Contributing to Macula

Thank you for considering contributing to Macula! This document outlines how to contribute effectively.

## Code of Conduct

Please read and follow our [Code of Conduct](CODE_OF_CONDUCT.md).

## Getting Started

1. Fork the repository
2. Clone your fork: `git clone https://github.com/YOUR_USERNAME/macula.git`
3. Add upstream remote: `git remote add upstream https://github.com/macula-io/macula.git`
4. Create a feature branch: `git checkout -b feature/your-feature`

## Development Setup

```bash
# Install Erlang/OTP 26+
# Install rebar3

# Fetch dependencies
rebar3 get-deps

# Compile
rebar3 compile

# Run tests: the unit tests, then the fleet suites in test/overlay
rebar3 eunit
rebar3 ct

# The wire property tests: CBOR, frames, records and UCAN, over generated inputs
bash scripts/is_the_property_suite_running.sh

# The same eunit tests in parallel shards, for the run before a push.
# About half the wall clock, same tests, one result.
bash scripts/test-parallel.sh
```

### Before you push: the gate that is CI

```bash
bash scripts/is_tree_pushable.sh
```

It runs the steps of the gating workflows themselves - `.github/workflows/test.yml` and
`docs.yml`, including the doc-snippet check, whose escript lives in `macula-ci-images` at the sha `docs.yml` pins -
inside the image those workflows pin, as root. It exports `HEAD` with `git archive` and runs on that export, never on
your working directory: a file git ignores - a `_checkouts/<dep>` shadowing the locked one, an `_build` from another
branch - cannot make the gate disagree with CI, and the container's files land outside your checkout.

**It is derived from the workflows, not copied from them (macula#163).** The steps are read out of the workflow files
at run time, so a step CI gains is a step the gate gains, and the two cannot drift apart in silence. The thing that
used to happen instead is worth knowing, because it is why the script exists: a package deleted several exported
arities, the hand-written local gate reported eight green steps, and CI's docs job then went red because three runnable
examples in the guides still called the deleted functions. The gap was real, invisible locally, and found after the
work had been handed over.

Three things follow from that, and they are the rule:

- **`scripts/ci_gate_plan.sh` is the only place a job can be excluded.** A job that cannot run inside the build image
  (cargo clippy on a plain runner, the advisory scans) is ruled there with the reason a member is told. A job nobody
  has ruled on is a refusal, not a silent skip: a new job is a decision, and the decision belongs in a commit.
- **`scripts/is_the_local_gate_in_step_with_ci.sh` fails when the two drift**, and CI runs it. A ruling that outlives
  its job, a matrix check whose steps all skipped, a reusable workflow not pinned by a full sha - each is red there
  rather than discovered by the next member.
- **A green gate names what it does not cover.** The `NOT-RUN:` lines before the steps are the jobs and the workflows
  CI still runs and the gate does not - `rust-lint`, the advisory scans, the PDF build, the release - each with the
  reason. A job nobody has ruled on, a workflow file in `.github/workflows` in neither the plan nor the ruled list, a
  shell other than the default bash, a `${{ ... }}` the gate cannot resolve: each is a refusal, not a skip.
  Read those lines before believing a green gate is a green push.

It exits 3, never 0, when no container runtime is installed: a gate that could not run has not passed.

`scripts/test-parallel.sh` is also how to check that a test is honest rather
than merely passing. A test can be green because of the order or timing the
suite happens to give it; running it under the shards, and on its own with
`rebar3 eunit --module=<module>`, is what exposes that. Both are worth doing for
a test you have just written.

## Coding Standards

### Erlang Style

- Follow idiomatic Erlang patterns
- Use pattern matching on function heads instead of `case`/`if`
- Keep nesting to 1-2 levels maximum
- Prefer guards over conditional logic
- All exported functions must have `-spec` type specifications
- Add EDoc documentation to public functions

### Architecture Patterns

- `*_system` modules are supervisors ONLY (no business logic)
- Follow OTP supervision patterns
- Use CRDT for state that needs eventual consistency
- Events use past tense (facts that happened)
- RPC methods use present tense imperatives (commands)

### Testing

- Write tests first (TDD approach)
- Tests go in `test/` mirroring `src/` structure
- Use EUnit for unit tests, and common_test for a suite that runs several
  processes as a fleet (`test/overlay/`); CI runs both
- Use PropEr for a property about the wire — a value that must round-trip, a
  decoder that must answer whatever arrives, a signature that must not verify
  once a bit of it changes. Properties live in `test/prop_*.erl` and run with
  `rebar3 proper`; CI runs them, and fails when zero properties ran, so a
  renamed module is a red run rather than a check that silently tested nothing.
  A property is only as good as its generator: `scripts/is_the_property_suite_running.sh`
  and a deliberate break for a moment are how to see yours fail before you
  trust it.
- Clean up test fixtures properly (see `ensure_stopped/0` pattern)
- **A run passes on its exit code, never on the absence of "Failed".** A
  cancelled eunit test (a fixture whose setup crashed, a test past its
  timeout) prints `Failed: 0.  Skipped: 0.  Passed: N-1.` and
  `One or more tests were cancelled.`: no failure line, one test short.
  `rebar3 eunit` exits 1 on it, so trust the exit code, or read the summary
  for `All N tests passed.` with the N you expect. A script or filter that
  looks only for "Failed" reads that run as green (macula#43).
- **A skipped Common Test case fails the run** unless `rebar.config` allows
  it with a reason: `rebar3 ct` exits 0 on a case that skipped itself, so
  `cth_skip_is_failure` (from `macula_testkit`) turns an unallowed skip into
  a failure.
- **Line coverage has a floor, and it only goes up.** CI merges the eunit and
  ct suites' line coverage and fails below 82%
  (`scripts/is_line_coverage_at_least_the_floor.sh`). The floor is main's
  measured value rounded down to a whole percent: on 28 October 2026 that was
  just under 83%, so 82. Neither count is a property of the tree — the covered
  count moved by up to three lines across consecutive runs of one commit, and
  a rebase onto a new base moves both — so the job's own output is the
  authority for them. A whole percent here is about 131 lines, so a
  whole-percent floor cannot sit closer than that, and 82 leaves about a
  hundred covered lines of headroom. A change that adds more than
  that many untested lines is red here. A change that raises coverage raises
  the number in the same commit. Lowering it is a reviewed decision about
  which lines stop being tested, stated in the commit that does
  it; it is never done to turn a branch green. To see where a change stands:

  ```bash
  rebar3 eunit --cover
  rebar3 ct --cover
  bash scripts/is_line_coverage_at_least_the_floor.sh \
       _build/test/cover/eunit.coverdata _build/test/cover/ct.coverdata
  ```

  Both suites, merged: the floor is a property of the suite, not of one of
  its halves. The two are not equal contributors — eunit accounts for all but
  a couple of the covered lines today — but a number that silently leaves half
  the suite out is not the suite's coverage, and which half matters is not
  something to re-decide each time. The number is OTP's `cover` on the pinned
  Erlang, which is why CI asserts that pin before it measures.

## Submitting Changes

1. Ensure the tree is fit to push: `scripts/is_tree_pushable.sh` (a clean tree,
   the pinned Erlang/OTP, `rebar3 xref`, then every eunit test)
2. Run dialyzer as CI does, with the calls a consumer makes: `rebar3 as consumer_contracts dialyzer`
3. Generate docs to check for warnings: `rebar3 edoc`
4. Commit with clear messages
5. Push to your fork
6. Open a Pull Request

### Commit Messages

Use clear, descriptive commit messages:

```
Add QUIC connection pooling with LRU eviction

- Implement max connection limit (1000)
- Add LRU eviction when limit reached
- 22 tests for pool lifecycle
```

### Pull Request Guidelines

- Reference related issues
- Describe what changes you made and why
- Include test coverage for new functionality
- Update documentation if needed
- Update CHANGELOG.md under "Unreleased" section

## Reporting Issues

When reporting issues:

1. Check existing issues first
2. Provide clear reproduction steps
3. Include Erlang/OTP version
4. Include relevant logs or error messages

## Questions

For questions about the codebase:
- Open a GitHub Discussion
- Check existing documentation in `docs/` and `architecture/`

## License

By contributing, you agree that your contributions will be licensed under the Apache-2.0 License.
