use quinn::{ClientConfig, ServerConfig, TransportConfig, VarInt};
use rustls::pki_types::{CertificateDer, PrivateKeyDer};
use std::fs;
use std::net::{IpAddr, SocketAddr, UdpSocket};
use std::sync::Arc;
use std::time::Duration;

use crate::cert;

/// A stream's receive window when the listener sets none: the credit a peer
/// gets on one stream before this side reads.
///
/// Well above Quinn's conservative defaults (1.25MB stream, 1.25MB *
/// streams connection). Macula peering uses ONE long-lived bidi
/// stream per connection over which we multiplex pubsub EVENTs,
/// CALL/REPLY, DHT records, blob streams. With many small frames
/// in flight and the receiver doing per-frame verify (~200µs
/// Ed25519), the default 1.25MB window exhausts long before the
/// receiver acks consumed bytes — surfaces as receiver-bound
/// throughput in pubsub flood torture.
///
/// 16 MB stream window absorbs ~100k 150-byte EVENT frames before
/// backpressure; 64 MB connection window scales with our typical
/// 1-2 streams per peering. send_window matches.
pub const DEFAULT_STREAM_RECEIVE_WINDOW: u64 = 16 * 1024 * 1024;

/// A connection's receive window when the listener sets none: the credit a
/// peer gets across all of a connection's streams.
pub const DEFAULT_RECEIVE_WINDOW: u64 = 64 * 1024 * 1024;

/// Build a Quinn ServerConfig from Erlang options.
///
/// Required: certfile, keyfile
/// Optional: alpn (default ["macula"]), idle_timeout_ms, keep_alive_interval_ms,
///           peer_bidi_stream_count, peer_unidi_stream_count,
///           stream_receive_window, receive_window
pub fn build_server_config(
    certfile: &str,
    keyfile: &str,
    alpn: &[String],
    idle_timeout_ms: u64,
    keep_alive_ms: u64,
    bidi_streams: u32,
    uni_streams: u32,
    stream_receive_window: u64,
    receive_window: u64,
) -> Result<ServerConfig, String> {
    let certs = load_certs(certfile)?;
    let key = load_key(keyfile)?;

    let mut server_crypto = rustls::ServerConfig::builder()
        .with_no_client_auth()
        .with_single_cert(certs, key)
        .map_err(|e| format!("TLS config error: {}", e))?;

    server_crypto.alpn_protocols = alpn.iter().map(|s| s.as_bytes().to_vec()).collect();

    let mut transport = TransportConfig::default();
    transport.max_idle_timeout(Some(
        quinn::IdleTimeout::try_from(Duration::from_millis(idle_timeout_ms))
            .map_err(|e| format!("idle_timeout: {}", e))?,
    ));
    transport.keep_alive_interval(Some(Duration::from_millis(keep_alive_ms)));
    transport.max_concurrent_bidi_streams(bidi_streams.into());
    transport.max_concurrent_uni_streams(uni_streams.into());
    apply_flow_control(&mut transport, stream_receive_window, receive_window)?;

    let mut config =
        ServerConfig::with_crypto(Arc::new(
            quinn::crypto::rustls::QuicServerConfig::try_from(server_crypto)
                .map_err(|e| format!("QUIC server config: {}", e))?,
        ));
    config.transport_config(Arc::new(transport));

    Ok(config)
}

/// Sets the stream and connection receive windows, and a send window as large
/// as the default connection receive window.
fn apply_flow_control(
    transport: &mut TransportConfig,
    stream_receive_window: u64,
    receive_window: u64,
) -> Result<(), String> {
    let stream_window = VarInt::from_u64(stream_receive_window)
        .map_err(|_| format!("stream_receive_window too large: {}", stream_receive_window))?;
    let connection_window = VarInt::from_u64(receive_window)
        .map_err(|_| format!("receive_window too large: {}", receive_window))?;
    transport.stream_receive_window(stream_window);
    transport.receive_window(connection_window);
    transport.send_window(DEFAULT_RECEIVE_WINDOW);
    Ok(())
}

/// Build a Quinn ClientConfig.
///
/// Three trust modes:
///   - `pinned_pubkey = Some(pk)` — pubkey-anchored (sovereign overlay
///     path). Validates the leaf cert's Ed25519 SubjectPublicKeyInfo
///     against `pk`. No CA chain. See PLAN_SOVEREIGN_OVERLAY_PHASE1
///     §4.4.
///   - `pinned_pubkey = None`, `verify = true` — webpki + system CAs
///     (existing public-IP path with Let's Encrypt-anchored certs).
///   - `pinned_pubkey = None`, `verify = false` — skip all verification
///     (development/test only).
pub fn build_client_config(
    alpn: &[String],
    verify: bool,
    pinned_pubkey: Option<Vec<u8>>,
    idle_timeout_ms: u64,
    keep_alive_ms: u64,
) -> Result<ClientConfig, String> {
    let client_crypto = if let Some(pk) = pinned_pubkey {
        rustls::ClientConfig::builder()
            .dangerous()
            .with_custom_certificate_verifier(Arc::new(cert::PubkeyPinVerifier::new(pk)))
            .with_no_client_auth()
    } else if verify {
        let mut roots = rustls::RootCertStore::empty();
        roots.extend(webpki_roots::TLS_SERVER_ROOTS.iter().cloned());
        rustls::ClientConfig::builder()
            .with_root_certificates(roots)
            .with_no_client_auth()
    } else {
        rustls::ClientConfig::builder()
            .dangerous()
            .with_custom_certificate_verifier(Arc::new(SkipServerVerification::new()))
            .with_no_client_auth()
    };

    let mut crypto = client_crypto;
    crypto.alpn_protocols = alpn.iter().map(|s| s.as_bytes().to_vec()).collect();

    let mut transport = TransportConfig::default();
    transport.max_idle_timeout(Some(
        quinn::IdleTimeout::try_from(Duration::from_millis(idle_timeout_ms))
            .map_err(|e| format!("idle_timeout: {}", e))?,
    ));
    transport.keep_alive_interval(Some(Duration::from_millis(keep_alive_ms)));
    apply_flow_control(&mut transport, DEFAULT_STREAM_RECEIVE_WINDOW, DEFAULT_RECEIVE_WINDOW)?;

    let mut config = ClientConfig::new(Arc::new(
        quinn::crypto::rustls::QuicClientConfig::try_from(crypto)
            .map_err(|e| format!("QUIC client config: {}", e))?,
    ));
    config.transport_config(Arc::new(transport));

    Ok(config)
}

/// Create a UDP socket bound to a specific address.
/// Uses socket2 for SO_REUSEPORT and fine-grained control.
pub fn create_bound_socket(addr: IpAddr, port: u16) -> Result<UdpSocket, String> {
    let socket_addr = SocketAddr::new(addr, port);
    let domain = match addr {
        IpAddr::V4(_) => socket2::Domain::IPV4,
        IpAddr::V6(_) => socket2::Domain::IPV6,
    };

    let socket = socket2::Socket::new(domain, socket2::Type::DGRAM, Some(socket2::Protocol::UDP))
        .map_err(|e| format!("socket create: {}", e))?;

    socket
        .set_reuse_port(true)
        .map_err(|e| format!("SO_REUSEPORT: {}", e))?;
    socket
        .set_nonblocking(true)
        .map_err(|e| format!("nonblocking: {}", e))?;

    if addr.is_ipv6() {
        // [::] (unspecified) = dual-stack (accepts IPv4 + IPv6)
        // Specific IPv6 (e.g. fd00::1) = v6-only (per-identity binding)
        let v6_only = !addr.is_unspecified();
        socket
            .set_only_v6(v6_only)
            .map_err(|e| format!("IPV6_V6ONLY: {}", e))?;
    }

    socket
        .bind(&socket_addr.into())
        .map_err(|e| format!("bind {}:{}: {}", addr, port, e))?;

    Ok(socket.into())
}

// ── TLS helpers ────────────────────────────────────────────────

fn load_certs(path: &str) -> Result<Vec<CertificateDer<'static>>, String> {
    let data = fs::read(path).map_err(|e| format!("read cert {}: {}", path, e))?;
    rustls_pemfile::certs(&mut &data[..])
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| format!("parse cert {}: {}", path, e))
}

fn load_key(path: &str) -> Result<PrivateKeyDer<'static>, String> {
    let data = fs::read(path).map_err(|e| format!("read key {}: {}", path, e))?;
    rustls_pemfile::private_key(&mut &data[..])
        .map_err(|e| format!("parse key {}: {}", path, e))?
        .ok_or_else(|| format!("no private key found in {}", path))
}

// ── Development mode: skip TLS verification ────────────────────

#[derive(Debug)]
struct SkipServerVerification(Arc<rustls::crypto::CryptoProvider>);

impl SkipServerVerification {
    fn new() -> Self {
        Self(Arc::new(rustls::crypto::ring::default_provider()))
    }
}

impl rustls::client::danger::ServerCertVerifier for SkipServerVerification {
    fn verify_server_cert(
        &self,
        _end_entity: &CertificateDer<'_>,
        _intermediates: &[CertificateDer<'_>],
        _server_name: &rustls::pki_types::ServerName<'_>,
        _ocsp_response: &[u8],
        _now: rustls::pki_types::UnixTime,
    ) -> Result<rustls::client::danger::ServerCertVerified, rustls::Error> {
        Ok(rustls::client::danger::ServerCertVerified::assertion())
    }

    fn verify_tls12_signature(
        &self,
        _message: &[u8],
        _cert: &CertificateDer<'_>,
        _dss: &rustls::DigitallySignedStruct,
    ) -> Result<rustls::client::danger::HandshakeSignatureValid, rustls::Error> {
        Ok(rustls::client::danger::HandshakeSignatureValid::assertion())
    }

    fn verify_tls13_signature(
        &self,
        _message: &[u8],
        _cert: &CertificateDer<'_>,
        _dss: &rustls::DigitallySignedStruct,
    ) -> Result<rustls::client::danger::HandshakeSignatureValid, rustls::Error> {
        Ok(rustls::client::danger::HandshakeSignatureValid::assertion())
    }

    fn supported_verify_schemes(&self) -> Vec<rustls::SignatureScheme> {
        self.0.signature_verification_algorithms.supported_schemes()
    }
}
