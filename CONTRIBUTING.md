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

# Run tests
rebar3 eunit
```

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
- Use EUnit for unit tests
- Clean up test fixtures properly (see `ensure_stopped/0` pattern)

## Submitting Changes

1. Ensure all tests pass: `rebar3 eunit`
2. Run dialyzer: `rebar3 dialyzer`
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
