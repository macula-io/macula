<div align="center">
  <img src="artwork/macula-alt-logo.svg" alt="Macula Logo" width="500"/>

  <h1>Macula HTTP/3 Mesh</h1>
  <p><em>A distributed platform for decentralized applications</em></p>
</div>

<p align="center">
  <a href="LICENSE"><img src="https://img.shields.io/badge/License-Apache%202.0-blue.svg" alt="License"/></a>
  <a href="https://www.erlang.org"><img src="https://img.shields.io/badge/Erlang%2FOTP-26+-brightgreen" alt="Erlang/OTP"/></a>
</p>

---

## Table of Contents

📖 **[Executive Summary](docs/EXECUTIVE_SUMMARY.md)** - What Macula is and why it matters

🚀 **[Quick Start](architecture/macula_http3_mesh_quick_start.md)** - Get running in 15 minutes

💡 **[Hello World Tutorial](HELLO_WORLD.md)** - Build a distributed chat app in 30 minutes

📚 **[Technical Documentation](architecture/macula_http3_mesh_root.md)** - Complete architecture and implementation guides

🔧 **[Development Guide](docs/DEVELOPMENT.md)** - Setup, testing, and contributing

🏗️ **[Comparisons](docs/COMPARISONS.md)** - How Macula compares to similar systems

📊 **[Use Cases](docs/USE_CASES.md)** - Real-world applications and examples

📝 **[Project Structure](architecture/MACULA_PROJECT_STRUCTURE.md)** - Module organization and dependencies

🗺️ **[Roadmap](architecture/macula_http3_mesh_roadmap.md)** - 20-week implementation plan

📄 **[Changelog](CHANGELOG.md)** - Version history and migration guides

---

## What is Macula?

Macula is infrastructure for building **decentralized applications and services** that operate autonomously at the edge, without dependency on centralized cloud infrastructure.

**Key Features:**

✅ **BEAM-native** (Erlang/Elixir OTP supervision and fault tolerance)
✅ **HTTP/3 (QUIC)** transport (modern, encrypted, NAT-friendly)
✅ **Edge-first design** (works through firewalls and NAT)
✅ **Built-in pub/sub & RPC** (no external message broker needed)
✅ **Multi-tenancy** (realm isolation for SaaS and shared infrastructure)
✅ **Self-organizing mesh** (DHT-based service discovery, O(log N) routing)
✅ **Production-ready patterns** (OTP behaviors, comprehensive testing, memory management)

---

## Installation

**Elixir (mix.exs):**

```elixir
def deps do
  [
    {:macula, git: "https://github.com/macula-io/macula.git", branch: "main"}
  ]
end
```

**Erlang (rebar.config):**

```erlang
{deps, [
    {macula, {git, "https://github.com/macula-io/macula.git", {branch, "main"}}}
]}.
```

---

## Development Setup

```bash
# Clone the repository
git clone https://github.com/macula-io/macula.git
cd macula

# Fetch dependencies
rebar3 get-deps

# Compile
rebar3 compile

# Run tests
rebar3 eunit

# Start a shell with Macula loaded
rebar3 shell
```

---

## Contributing

We welcome contributions! See [Development Guide](docs/DEVELOPMENT.md) for setup instructions and [Contributing Guide](CONTRIBUTING.md) for guidelines.

---

## License

Macula is licensed under the Apache License 2.0. See [LICENSE](LICENSE) for details.

---

## Community & Support

- **Issues**: [GitHub Issues](https://github.com/macula-io/macula/issues)
- **Documentation**: [Architecture Docs](architecture/)
- **Examples**: [Examples Directory](examples/)

---

**Built with ❤️ for the BEAM community**
