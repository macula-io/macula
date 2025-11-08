# Macula HTTP/3 Mesh

**A distributed mesh networking platform for the BEAM, built on HTTP/3/QUIC**

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)
[![Erlang/OTP](https://img.shields.io/badge/Erlang%2FOTP-26+-brightgreen)](https://www.erlang.org)

---

## Vision

Macula is a unique, standards-based distributed networking layer for Erlang/Elixir applications that:
- Uses **HTTP/3 (QUIC)** for NAT-friendly, firewall-friendly transport
- Forms **self-organizing mesh topologies** at the edge
- Provides **pub/sub and RPC primitives**
- Scales to **thousands of nodes**
- Supports **multi-tenancy and realm isolation**
- Delivers the **"Wow! How do they do it?"** factor

---

## Quick Start

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

See [Quick Start Guide](architecture/macula_http3_mesh_quick_start.md) for detailed instructions.

---

## Architecture

Macula is organized as an Erlang umbrella application with 12 core libraries.

See [Project Structure](architecture/MACULA_PROJECT_STRUCTURE.md) for complete details.

---

## Documentation

📚 **Start Here**: [Documentation Root](architecture/macula_http3_mesh_root.md)

### Essential Reading

- [Quick Start Guide](architecture/macula_http3_mesh_quick_start.md) - Get running in 15 minutes
- [Hello World Tutorial](architecture/macula_http3_mesh_hello_world.md) - Build a distributed chat app
- [Technical Roadmap](architecture/macula_http3_mesh_roadmap.md) - 20-week implementation plan

See [Documentation Status](architecture/DOCUMENTATION_STATUS.md) for full inventory.

---

## Development Status

**Current Phase**: Foundation (Weeks 1-4)

**Completion**:
- ✅ Architecture design complete
- ✅ Project structure initialized
- ✅ Documentation comprehensive (34% complete, 6/21 docs)
- 🚧 Core libraries implementation in progress

---

## Contributing

We welcome contributions! See [Contributing Guide](architecture/macula_http3_mesh_contributing.md).

---

## License

Macula is licensed under the Apache License 2.0. See [LICENSE](LICENSE) for details.

---

**Built with ❤️ by the Macula team**
