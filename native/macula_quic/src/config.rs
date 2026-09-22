use quinn::{ClientConfig, ServerConfig, TransportConfig, VarInt};
use rustls::crypto::CryptoProvider;
use rustls::pki_types::{CertificateDer, PrivateKeyDer};
use rustls::sign::CertifiedKey;
use std::fs;
use std::net::{IpAddr, SocketAddr, UdpSocket};
use std::sync::Arc;
use std::time::Duration;

/// What a listener was started with, apart from its certificate, so that a
/// certificate reload builds the next server configuration the same way.
pub struct ServerSettings {
    pub alpn: Vec<String>,
    pub idle_timeout_ms: u64,
    pub keep_alive_ms: u64,
    pub bidi_streams: u32,
    pub uni_streams: u32,
    pub stream_receive_window: u64,
    pub receive_window: u64,
}

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

/// Build a Quinn ServerConfig from a certificate file, a key file and a
/// listener's settings, including its stream and connection receive windows.
/// Returns it with the leaf certificate it presents, as DER.
///
/// Refuses a key that does not match the leaf certificate: a listener must
/// never present a leaf its key cannot sign for.
pub fn build_server_config(
    certfile: &str,
    keyfile: &str,
    settings: &ServerSettings,
) -> Result<(ServerConfig, CertificateDer<'static>), String> {
    let certs = load_certs(certfile)?;
    let key = load_key(keyfile)?;
    let leaf = certs
        .first()
        .cloned()
        .ok_or_else(|| format!("no certificate found in {}", certfile))?;

    let server_crypto = server_tls_config(certs, key, &settings.alpn)?;

    let mut transport = TransportConfig::default();
    transport.max_idle_timeout(Some(
        quinn::IdleTimeout::try_from(Duration::from_millis(settings.idle_timeout_ms))
            .map_err(|e| format!("idle_timeout: {}", e))?,
    ));
    transport.keep_alive_interval(Some(Duration::from_millis(settings.keep_alive_ms)));
    transport.max_concurrent_bidi_streams(settings.bidi_streams.into());
    transport.max_concurrent_uni_streams(settings.uni_streams.into());
    apply_flow_control(&mut transport, settings.stream_receive_window, settings.receive_window)?;

    let mut config =
        ServerConfig::with_crypto(Arc::new(
            quinn::crypto::rustls::QuicServerConfig::try_from(server_crypto)
                .map_err(|e| format!("QUIC server config: {}", e))?,
        ));
    config.transport_config(Arc::new(transport));

    Ok((config, leaf))
}

/// The rustls half of a listener's configuration, before Quinn wraps it.
///
/// Split out from `build_server_config` so that a test drives the
/// configuration this NIF actually serves with, rather than a second copy of
/// it built the same way.
///
/// WHICH KEY EXCHANGE GROUPS AND SIGNATURES A HANDSHAKE MAY USE IS DECIDED BY
/// `macula-pqc`, not here: its builder arrives with the groups, ML-DSA-87 and
/// TLS 1.3 already fixed, and keeps its provider where nothing here can edit
/// it. So the certificate and key must be ML-DSA-87, as `cert.rs` makes them.
/// See the conditions on those lists in `macula-pqc`'s documentation, and the
/// tests below, which assert what this NIF actually offers.
pub fn server_tls_config(
    certs: Vec<CertificateDer<'static>>,
    key: PrivateKeyDer<'static>,
    alpn: &[String],
) -> Result<rustls::ServerConfig, String> {
    let builder = macula_pqc::server_builder();
    check_key_matches_leaf(&certs, &key, builder.crypto_provider())?;
    let mut crypto = builder
        .with_no_client_auth()
        .with_single_cert(certs, key)
        .map_err(|e| format!("TLS config error: {}", e))?;
    crypto.alpn_protocols = alpn.iter().map(|s| s.as_bytes().to_vec()).collect();
    Ok(crypto)
}

/// The rustls half of a dial's configuration, before Quinn wraps it. See
/// `server_tls_config` for why this is a seam rather than inlined.
///
/// ONE VERIFICATION MODE (plan decisions D12 and D16): the station presents
/// one self-signed ML-DSA-87 certificate, and the dial checks the station's
/// TLS 1.3 handshake signature under that certificate's key, with
/// `macula-pqc`'s `KeyPossessionVerifier`. That proves the station holds the
/// key and nothing more. Who the station is, is proved by the connection
/// handshake, which checks the station identity's binding over this key
/// against the leaf this connection received (`nif_peer_leaf`). There is no
/// certificate authority to consult: none issues ML-DSA certificates.
pub fn client_tls_config(alpn: &[String]) -> rustls::ClientConfig {
    let mut crypto = macula_pqc::client_builder()
        .dangerous()
        .with_custom_certificate_verifier(Arc::new(macula_pqc::KeyPossessionVerifier::new()))
        .with_no_client_auth();
    crypto.alpn_protocols = alpn.iter().map(|s| s.as_bytes().to_vec()).collect();
    crypto
}

/// Refuses a private key whose public key is not the leaf certificate's,
/// and a key that cannot state its public key, so the pair is known to
/// match rather than only not known to differ.
fn check_key_matches_leaf(
    certs: &[CertificateDer<'static>],
    key: &PrivateKeyDer<'static>,
    provider: &CryptoProvider,
) -> Result<(), String> {
    let signing_key = provider
        .key_provider
        .load_private_key(key.clone_key())
        .map_err(|e| format!("load private key: {}", e))?;
    CertifiedKey::new(certs.to_vec(), signing_key)
        .keys_match()
        .map_err(|e| format!("certificate and key do not match: {}", e))
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

/// Build a Quinn ClientConfig, verifying the station as `client_tls_config`
/// describes.
pub fn build_client_config(
    alpn: &[String],
    idle_timeout_ms: u64,
    keep_alive_ms: u64,
) -> Result<ClientConfig, String> {
    let crypto = client_tls_config(alpn);

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

// ── Tests ──────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use rustls::pki_types::ServerName;
    use rustls::sign::SingleCertAndKey;
    use rustls::{ClientConnection, Connection, NamedGroup, ServerConnection, SignatureScheme};

    use crate::cert;

    /// The key exchange groups whose shared secret a quantum computer cannot
    /// recover from the wire: the ML-KEM groups and the hybrids that carry
    /// one. Named rather than derived from the code point, so a future
    /// classical group numbered above the ML-KEM block cannot quietly count
    /// as post-quantum.
    fn is_post_quantum(group: NamedGroup) -> bool {
        group == SECP384R1MLKEM1024
            || matches!(
                group,
                NamedGroup::MLKEM512
                    | NamedGroup::MLKEM768
                    | NamedGroup::MLKEM1024
                    | NamedGroup::secp256r1MLKEM768
                    | NamedGroup::X25519MLKEM768
            )
    }

    /// `SecP384r1MLKEM1024`, code point `0x11ED`. rustls has no variant for
    /// it and no rustls provider ships it: only `macula-pqc` does.
    const SECP384R1MLKEM1024: NamedGroup = NamedGroup::Unknown(0x11ED);

    /// Exactly `macula-pqc`'s two groups, in its order.
    fn macula_pqc_groups() -> Vec<NamedGroup> {
        vec![SECP384R1MLKEM1024, NamedGroup::secp256r1MLKEM768]
    }

    fn offered(provider: &CryptoProvider) -> Vec<NamedGroup> {
        provider.kx_groups.iter().map(|g| g.name()).collect()
    }

    /// `macula-pqc`'s provider, read back from one of its builders: the only
    /// way to hold it outside that crate. The peers below take its signature
    /// verification and key loading, so what differs from ours is only what
    /// each test changes.
    fn facade_provider() -> CryptoProvider {
        (**macula_pqc::server_builder().crypto_provider()).clone()
    }

    /// EVERY configuration this NIF builds takes its groups from
    /// `macula-pqc`: the listener's and the dialler's. A configuration that
    /// built its own provider would be the one path a classical group could
    /// come back through.
    #[test]
    fn every_configuration_offers_exactly_macula_pqcs_groups() {
        let alpn = vec!["macula".to_string()];
        let (certs, key) = test_identity();
        let server = server_tls_config(certs, key, &alpn).expect("server config");
        let client = client_tls_config(&alpn);
        assert_eq!(offered(server.crypto_provider()), macula_pqc_groups(), "listener");
        assert_eq!(offered(client.crypto_provider()), macula_pqc_groups(), "dialler");
    }

    /// And every configuration signs and verifies ML-DSA-87 alone, the
    /// listener's handshake signature and the dialler's check of it. A
    /// classical scheme here would be a classical signature on the wire.
    #[test]
    fn every_configuration_verifies_ml_dsa_87_alone() {
        let alpn = vec!["macula".to_string()];
        let (certs, key) = test_identity();
        let server = server_tls_config(certs, key, &alpn).expect("server config");
        let client = client_tls_config(&alpn);
        for (side, provider) in [("listener", server.crypto_provider()), ("dialler", client.crypto_provider())] {
            assert_eq!(
                provider.signature_verification_algorithms.supported_schemes(),
                vec![SignatureScheme::ML_DSA_87],
                "{side}"
            );
        }
    }

    /// THE DELIVERABLE. Two endpoints, a real TLS 1.3 handshake, and an
    /// assertion on WHICH KEY EXCHANGE GROUP THEY AGREED ON.
    ///
    /// ⚠ It deliberately does NOT assert that the handshake succeeded. A
    /// handshake succeeding is the adjacent object: it succeeds just as
    /// happily over X25519, which is exactly the state this change exists to
    /// leave. The negotiated group is the artefact.
    ///
    /// The configurations are the NIF's own (`server_tls_config`,
    /// `client_tls_config`), not copies built the same way here, so what is
    /// asserted is what this NIF will serve and dial with.
    ///
    /// ⛔ DO NOT TRY TO MOVE THIS ASSERTION ONTO A REAL QUIC CONNECTION. It
    /// is the obvious improvement and it cannot be done with our
    /// dependencies. `rustls` has `CommonState::negotiated_key_exchange_group`
    /// (0.23.43), but `quinn_proto::crypto::rustls::TlsSession` keeps its
    /// rustls connection in a PRIVATE field and its `crypto::Session`
    /// implementation surfaces only the ALPN protocol and the server name
    /// through `handshake_data()`. No live QUIC connection in this stack can
    /// be asked what it negotiated.
    ///
    /// So this proves THE SDK'S TLS CONFIGURATION negotiates a post-quantum
    /// group. It does not prove a QUIC connection did. The Erlang side proves
    /// that differently and without needing the value: offer post-quantum
    /// groups only, and show a classical-only peer CANNOT connect, so a
    /// connection that does come up cannot have landed on a classical group.
    /// See `macula_quic_pq_kx_tests`.
    #[test]
    fn negotiated_key_exchange_group_is_post_quantum() {
        let group = negotiated_group();
        assert!(
            is_post_quantum(group),
            "negotiated key exchange group is {:?}, which is classical: a \
             recorded handshake stays decryptable by a quantum adversary",
            group
        );
    }

    /// WHICH post-quantum group, not merely that it is one.
    ///
    /// The preference order is `macula-pqc`'s, and a decision rather than an
    /// accident: `SecP384r1MLKEM1024` leads because it is the group the
    /// `pq_hybrid` profile declares, so two peers on this NIF negotiate it.
    /// Pinning the negotiated value here makes that decision something a
    /// change has to face rather than something a reordering can quietly
    /// undo. No other rustls provider has this group, so negotiating it also
    /// shows the handshake ran on `macula-pqc`'s key exchange.
    ///
    /// If the order is changed deliberately, change this with it and say why.
    #[test]
    fn negotiated_key_exchange_group_is_the_one_we_lead_with() {
        assert_eq!(negotiated_group(), SECP384R1MLKEM1024);
    }

    /// `aws-lc-rs`'s post-quantum groups, as this NIF offered them at macula
    /// `c91e0214`, before it moved onto `macula-pqc`. That list was never
    /// released: 11.5.0 and earlier negotiate classical groups and cannot
    /// connect to this NIF at all, which the negative control below models.
    ///
    /// A peer on it has no `SecP384r1MLKEM1024`, so the two must agree on
    /// `SecP256r1MLKEM768`, in both roles: `macula-pqc`'s ML-KEM against
    /// `aws-lc-rs`'s, through this NIF's own configurations. A DIFFERENTIAL
    /// CHECK, green before the move and after. The peer signs and verifies
    /// as we do, so the groups are the only difference.
    fn aws_lc_rs_post_quantum_list() -> CryptoProvider {
        CryptoProvider {
            kx_groups: vec![
                rustls::crypto::aws_lc_rs::kx_group::SECP256R1MLKEM768,
                rustls::crypto::aws_lc_rs::kx_group::X25519MLKEM768,
                rustls::crypto::aws_lc_rs::kx_group::MLKEM1024,
                rustls::crypto::aws_lc_rs::kx_group::MLKEM768,
            ],
            ..facade_provider()
        }
    }

    #[test]
    fn a_peer_on_aws_lc_rs_post_quantum_groups_agrees_on_secp256r1mlkem768() {
        let alpn = vec!["macula".to_string()];
        let (certs, key) = test_identity();

        let we_dial = handshake(
            client_tls_config(&alpn),
            peer_server(aws_lc_rs_post_quantum_list(), certs.clone(), key.clone_key()),
        );
        let they_dial = handshake(
            peer_client(aws_lc_rs_post_quantum_list()),
            server_tls_config(certs, key, &alpn).expect("ours"),
        );

        assert_eq!(we_dial, Ok(NamedGroup::secp256r1MLKEM768), "we dial an aws-lc-rs peer");
        assert_eq!(they_dial, Ok(NamedGroup::secp256r1MLKEM768), "an aws-lc-rs peer dials us");
    }

    /// THE NEGATIVE CONTROL, and without it the two tests above are worth
    /// much less than they look.
    ///
    /// They show a post-quantum group being negotiated. They do NOT show that
    /// the group list is doing any work: if `kx_groups` were inert and the
    /// provider were quietly falling back to its defaults, a handshake would
    /// still come up, and with `X25519MLKEM768` in the default list it might
    /// still come up post-quantum. An inert setting that produces the right
    /// answer by luck is the defect that hides longest.
    ///
    /// So: a client offering only our post-quantum groups, against a server
    /// offering only classical ones, MUST FAIL TO AGREE. It can only fail if
    /// both lists are genuinely in force.
    #[test]
    fn a_classical_only_peer_cannot_agree_with_us() {
        let alpn = vec!["macula".to_string()];
        let classical = || CryptoProvider {
            kx_groups: vec![
                rustls::crypto::aws_lc_rs::kx_group::X25519,
                rustls::crypto::aws_lc_rs::kx_group::SECP256R1,
                rustls::crypto::aws_lc_rs::kx_group::SECP384R1,
            ],
            ..facade_provider()
        };
        let (certs, key) = test_identity();
        let we_dial = handshake(
            client_tls_config(&alpn),
            peer_server(classical(), certs.clone(), key.clone_key()),
        );
        let they_dial = handshake(peer_client(classical()), server_tls_config(certs, key, &alpn).expect("ours"));

        assert!(
            we_dial.is_err(),
            "a classical-only server agreed with us, so our key exchange group \
             list is not in force: the post-quantum group the other tests see \
             is luck, not policy"
        );
        assert!(
            they_dial.is_err(),
            "a classical-only client agreed with our listener, so its key \
             exchange group list is not in force"
        );
    }

    /// ⛔ THE DIALLER'S NEGATIVE CONTROL. The dial trusts no certificate
    /// authority, so the station's handshake signature under its
    /// certificate's key is the one thing TLS proves for it. A station
    /// presenting one ML-DSA-87 certificate and signing with another
    /// ML-DSA-87 key must fail: a dialler that let that through would pass
    /// every other test in this file. The positive twin, the same harness
    /// with the certificate's own key, completes.
    #[test]
    fn the_dialler_refuses_a_station_that_cannot_sign_for_its_certificate() {
        let alpn = vec!["macula".to_string()];
        let (certs, key) = test_identity();
        let (_other_certs, other_key) = cert::generate_self_signed(&[3u8; 32], &["localhost".to_string()])
            .expect("another identity");
        let wrong_key = handshake(client_tls_config(&alpn), presenting(certs.clone(), other_key.into()));
        assert!(
            wrong_key.is_err(),
            "a station signing with a key other than its certificate's was accepted: {wrong_key:?}"
        );
        assert_eq!(
            handshake(client_tls_config(&alpn), presenting(certs, key)),
            Ok(SECP384R1MLKEM1024)
        );
    }

    /// A station with a classical certificate, as every 11.x station has, is
    /// refused: it cannot sign anything this dialler verifies.
    #[test]
    fn the_dialler_refuses_a_classical_station() {
        let alpn = vec!["macula".to_string()];
        let key = rcgen::KeyPair::generate().expect("ECDSA key");
        let certificate = rcgen::CertificateParams::new(vec!["localhost".to_string()])
            .expect("names")
            .self_signed(&key)
            .expect("ECDSA certificate");
        let classical_station = rustls::ServerConfig::builder_with_provider(Arc::new(
            rustls::crypto::aws_lc_rs::default_provider(),
        ))
        .with_safe_default_protocol_versions()
        .expect("versions")
        .with_no_client_auth()
        .with_single_cert(
            vec![certificate.der().clone()],
            rustls::pki_types::PrivatePkcs8KeyDer::from(key.serialize_der()).into(),
        )
        .expect("classical station");
        let agreed = handshake(client_tls_config(&alpn), classical_station);
        assert!(agreed.is_err(), "a station with an ECDSA certificate was accepted: {agreed:?}");
    }

    /// Runs one handshake between the NIF's own server and client
    /// configurations and returns the group they agreed on.
    fn negotiated_group() -> NamedGroup {
        let alpn = vec!["macula".to_string()];
        let (certs, key) = test_identity();
        let server_cfg = server_tls_config(certs, key, &alpn).expect("server config");
        handshake(client_tls_config(&alpn), server_cfg)
            .expect("the NIF's own configurations agree with each other")
    }

    /// A station identity, made by the NIF's own generator so the
    /// certificate under test is the kind it really issues.
    fn test_identity() -> (Vec<CertificateDer<'static>>, PrivateKeyDer<'static>) {
        let (certificate, key) =
            cert::generate_self_signed(&[9u8; 32], &["localhost".to_string()]).expect("self-signed cert");
        (vec![certificate], key.into())
    }

    /// A server on another provider, presenting `certs`.
    fn peer_server(
        provider: CryptoProvider,
        certs: Vec<CertificateDer<'static>>,
        key: PrivateKeyDer<'static>,
    ) -> rustls::ServerConfig {
        let mut config = rustls::ServerConfig::builder_with_provider(Arc::new(provider))
            .with_safe_default_protocol_versions()
            .expect("versions")
            .with_no_client_auth()
            .with_single_cert(certs, key)
            .expect("server config");
        config.alpn_protocols = vec![b"macula".to_vec()];
        config
    }

    /// A client on another provider, trusting a station as ours does.
    fn peer_client(provider: CryptoProvider) -> rustls::ClientConfig {
        let mut config = rustls::ClientConfig::builder_with_provider(Arc::new(provider))
            .with_safe_default_protocol_versions()
            .expect("versions")
            .dangerous()
            .with_custom_certificate_verifier(Arc::new(macula_pqc::KeyPossessionVerifier::new()))
            .with_no_client_auth();
        config.alpn_protocols = vec![b"macula".to_vec()];
        config
    }

    /// A station on `macula-pqc`'s builder presenting `certs` and signing
    /// with `key`, whatever the two are: a resolver skips the check
    /// `with_single_cert` makes that the key is the certificate's.
    fn presenting(certs: Vec<CertificateDer<'static>>, key: PrivateKeyDer<'static>) -> rustls::ServerConfig {
        let signing_key = facade_provider()
            .key_provider
            .load_private_key(key)
            .expect("an ML-DSA-87 key");
        let mut config = macula_pqc::server_builder()
            .with_no_client_auth()
            .with_cert_resolver(Arc::new(SingleCertAndKey::from(CertifiedKey::new(certs, signing_key))));
        config.alpn_protocols = vec![b"macula".to_vec()];
        config
    }

    /// One real handshake, returning the group the two sides agreed on or the
    /// reason they could not. A failure to AGREE is a result here, not an
    /// error: it is what the negative controls assert.
    fn handshake(
        client_cfg: rustls::ClientConfig,
        server_cfg: rustls::ServerConfig,
    ) -> Result<NamedGroup, String> {
        let mut client = Connection::Client(
            ClientConnection::new(
                Arc::new(client_cfg),
                ServerName::try_from("localhost").unwrap(),
            )
            .map_err(|e| format!("client connection: {}", e))?,
        );
        let mut server = Connection::Server(
            ServerConnection::new(Arc::new(server_cfg))
                .map_err(|e| format!("server connection: {}", e))?,
        );

        // Twenty flights is far more than TLS 1.3 needs and bounds a
        // handshake that stops progressing.
        for _ in 0..20 {
            let client_moved = pump(&mut client, &mut server)?;
            let server_moved = pump(&mut server, &mut client)?;
            if client_moved + server_moved == 0 && !client.is_handshaking() {
                break;
            }
        }

        if client.is_handshaking() {
            return Err("handshake never completed".to_string());
        }
        client
            .negotiated_key_exchange_group()
            .map(|g| g.name())
            .ok_or_else(|| "completed with no key exchange group".to_string())
    }

    /// Moves whatever `from` wants to write into `to`, and returns how many
    /// bytes crossed, so the caller can tell a stalled handshake from a
    /// finished one.
    fn pump(from: &mut Connection, to: &mut Connection) -> Result<usize, String> {
        let mut buf = Vec::new();
        while from.wants_write() {
            from.write_tls(&mut buf)
                .map_err(|e| format!("write_tls: {}", e))?;
        }
        if buf.is_empty() {
            return Ok(0);
        }
        let mut cursor = std::io::Cursor::new(&buf[..]);
        while (cursor.position() as usize) < buf.len() {
            to.read_tls(&mut cursor)
                .map_err(|e| format!("read_tls: {}", e))?;
            to.process_new_packets()
                .map_err(|e| format!("process_new_packets: {}", e))?;
        }
        Ok(buf.len())
    }
}
