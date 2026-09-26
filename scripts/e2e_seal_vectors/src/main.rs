//! Writes the E2E seal scheme 1 vectors (test/vectors/E2E_SEAL_V1.md) as JSON on stdout.
//!
//! Every input that is random in use (key seeds, the ML-KEM encapsulation
//! seed `m`, ephemeral P-384 scalars, carried nonces, group keys) is fixed
//! here as the first bytes of SHA-384("macula e2e seal v1: " || name), so the
//! file is reproducible and anyone can regenerate it.

use aes_gcm::aead::{Aead, KeyInit, Payload};
use aes_gcm::{Aes256Gcm, Nonce};
use ciborium::value::Value;
use hkdf::Hkdf;
use macula_mlkem::{internal, ML_KEM_1024};
use p384::elliptic_curve::sec1::ToEncodedPoint;
use p384::{ecdh, PublicKey, SecretKey};
use serde_json::{json, Value as Json};
use sha2::{Digest, Sha384};

fn fixed(name: &str, len: usize) -> Vec<u8> {
    let mut out = Vec::new();
    let mut counter = 0u32;
    while out.len() < len {
        let mut h = Sha384::new();
        h.update(format!("macula e2e seal v1: {name}#{counter}").as_bytes());
        out.extend_from_slice(&h.finalize());
        counter += 1;
    }
    out.truncate(len);
    out
}

fn arr<const N: usize>(v: &[u8]) -> [u8; N] {
    v.try_into().expect("length")
}

fn cbor(items: Vec<Value>) -> Vec<u8> {
    let mut out = Vec::new();
    ciborium::ser::into_writer(&Value::Array(items), &mut out).expect("cbor");
    out
}

fn b(v: &[u8]) -> Value {
    Value::Bytes(v.to_vec())
}

fn t(s: &str) -> Value {
    Value::Text(s.to_string())
}

fn u(n: u64) -> Value {
    Value::Integer(n.into())
}

fn extract(salt: &[u8], ikm: &[u8]) -> Vec<u8> {
    let (prk, _) = Hkdf::<Sha384>::extract(Some(salt), ikm);
    prk.to_vec()
}

fn expand(prk: &[u8], info: &[u8], len: usize) -> Vec<u8> {
    let mut okm = vec![0u8; len];
    Hkdf::<Sha384>::from_prk(prk).expect("prk").expand(info, &mut okm).expect("expand");
    okm
}

fn seal(key: &[u8], nonce: &[u8], aad: &[u8], pt: &[u8]) -> Vec<u8> {
    Aes256Gcm::new_from_slice(key)
        .expect("key")
        .encrypt(Nonce::from_slice(nonce), Payload { msg: pt, aad })
        .expect("seal")
}

fn h(v: &[u8]) -> String {
    hex::encode(v)
}

fn sha384(v: &[u8]) -> Vec<u8> {
    Sha384::digest(v).to_vec()
}

struct Recipient {
    json: Json,
    ek: Vec<u8>,
    p384_pub: Option<PublicKey>,
    key_hash: Vec<u8>,
    key_id: Vec<u8>,
}

fn recipient(profile: &str) -> Recipient {
    let d = fixed(&format!("{profile} recipient d"), 32);
    let z = fixed(&format!("{profile} recipient z"), 32);
    let (ek, dk) = internal::key_gen(ML_KEM_1024, &arr(&d), &arr(&z));
    let mut carried = ek.clone();
    let mut json = json!({
        "mlkem_seed": h(&[d.as_slice(), z.as_slice()].concat()),
        "mlkem_ek": h(&ek),
        "mlkem_dk": h(&dk),
    });
    let p384_pub = (profile == "pq_hybrid").then(|| {
        let sk = SecretKey::from_slice(&fixed("pq_hybrid recipient p384", 48)).expect("scalar");
        let pk = sk.public_key();
        json["p384_priv"] = json!(h(&sk.to_bytes()));
        json["p384_pub"] = json!(h(pk.to_encoded_point(false).as_bytes()));
        carried.extend_from_slice(pk.to_encoded_point(false).as_bytes());
        pk
    });
    let key_hash = sha384(&carried);
    let key_id = key_hash[..8].to_vec();
    json["key_as_carried"] = json!(h(&carried));
    json["key_hash"] = json!(h(&key_hash));
    json["key_id"] = json!(h(&key_id));
    Recipient { json, ek, p384_pub, key_hash, key_id }
}

/// The shared secret for one encapsulation, and the vector's record of it.
fn shared(profile: &str, r: &Recipient, name: &str) -> (Vec<u8>, Vec<u8>, Json) {
    let m = fixed(&format!("{name} m"), 32);
    let (mlkem_ct, ss_mlkem) = internal::encaps(ML_KEM_1024, &r.ek, &arr(&m)).expect("encaps");
    let mut out = json!({
        "encaps_m": h(&m),
        "ss_mlkem": h(&*ss_mlkem),
        "mlkem_ct": h(&mlkem_ct),
    });
    let (salt, ikm, kem_ct) = match &r.p384_pub {
        None => (
            "MACULA-E2E-PURE-V1",
            cbor(vec![b(&*ss_mlkem), b(&mlkem_ct), b(&r.key_hash)]),
            mlkem_ct.clone(),
        ),
        Some(rpub) => {
            let eph = SecretKey::from_slice(&fixed(&format!("{name} eph"), 48)).expect("scalar");
            let eph_pub = eph.public_key().to_encoded_point(false).as_bytes().to_vec();
            let ss_ecdh = ecdh::diffie_hellman(eph.to_nonzero_scalar(), rpub.as_affine());
            let ss_ecdh = ss_ecdh.raw_secret_bytes().to_vec();
            out["eph_priv"] = json!(h(&eph.to_bytes()));
            out["eph_pub"] = json!(h(&eph_pub));
            out["ss_ecdh"] = json!(h(&ss_ecdh));
            (
                "MACULA-E2E-HYBRID-V1",
                cbor(vec![b(&*ss_mlkem), b(&ss_ecdh), b(&mlkem_ct), b(&eph_pub), b(&r.key_hash)]),
                [mlkem_ct.as_slice(), eph_pub.as_slice()].concat(),
            )
        }
    };
    let ss = extract(salt.as_bytes(), &ikm);
    out["profile"] = json!(profile);
    out["ikm"] = json!(h(&ikm));
    out["kem_ct"] = json!(h(&kem_ct));
    out["ss"] = json!(h(&ss));
    (ss, kem_ct, out)
}

fn call_vector(profile: &str, r: &Recipient, frame_type: &str) -> Json {
    let name = format!("{profile} {frame_type}");
    let (ss, _kem_ct, mut v) = shared(profile, r, &name);
    let request_id = fixed(&format!("{name} request_id"), 16);
    let caller = fixed(&format!("{name} caller"), 32);
    let target = fixed(&format!("{name} target"), 32);
    let realm = fixed(&format!("{name} realm"), 32);
    let procedure = "acme/count_v1";
    let deadline: u64 = 1_790_000_000_000;
    let okm = expand(&ss, &cbor(vec![t("MACULA-E2E-CALL-V1"), t(frame_type), b(&request_id), b(&caller), b(&target)]), 64);
    let (k_req, k_rep) = okm.split_at(32);
    let request_plain = fixed(&format!("{name} request payload"), 41);
    let request_aad = cbor(vec![
        t("MACULA-E2E-AAD-V1"), t(frame_type), b(&realm), t(procedure), b(&caller), b(&target), b(&request_id), u(deadline),
    ]);
    let request_ct = seal(k_req, &[0u8; 12], &request_aad, &request_plain);
    let request_hash = fixed(&format!("{name} request_hash"), 48);
    let reply_nonce = fixed(&format!("{name} reply nonce"), 12);
    let reply_plain = fixed(&format!("{name} reply payload"), 23);
    let reply_type = "result";
    let reply_aad = cbor(vec![
        t("MACULA-E2E-AAD-V1"), t(reply_type), b(&realm), t(procedure), b(&caller), b(&target), b(&request_id), u(deadline),
        b(&request_hash), b(&target),
    ]);
    let reply_ct = seal(k_rep, &reply_nonce, &reply_aad, &reply_plain);
    v["frame_type"] = json!(frame_type);
    v["key_id"] = json!(h(&r.key_id));
    v["request_id"] = json!(h(&request_id));
    v["caller"] = json!(h(&caller));
    v["target"] = json!(h(&target));
    v["realm"] = json!(h(&realm));
    v["procedure"] = json!(procedure);
    v["deadline"] = json!(deadline);
    v["k_req"] = json!(h(k_req));
    v["k_rep"] = json!(h(k_rep));
    v["request"] = json!({"plain": h(&request_plain), "aad": h(&request_aad), "nonce": h(&[0u8; 12]), "ct": h(&request_ct)});
    if frame_type == "call" {
        v["reply"] = json!({
        "frame_type": reply_type, "request_hash": h(&request_hash), "responded_by": h(&target),
        "plain": h(&reply_plain), "aad": h(&reply_aad), "nonce": h(&reply_nonce), "ct": h(&reply_ct),
        });
    }
    if frame_type == "stream_open" {
        let okm = expand(&ss, &cbor(vec![t("MACULA-E2E-STREAM-V1"), b(&request_id), b(&caller), b(&target)]), 64);
        let (k_c2p, k_p2c) = okm.split_at(32);
        let mut frames = Vec::new();
        for (dir, seq, ftype) in [(0u64, 0u64, "stream_data"), (0, 1, "stream_data"), (1, 0, "stream_data"), (1, 1, "stream_reply")] {
            let key = if dir == 0 { k_c2p } else { k_p2c };
            let nonce = if dir == 0 {
                let mut n = vec![0u8; 12];
                n[4..].copy_from_slice(&seq.to_be_bytes());
                n
            } else {
                fixed(&format!("{name} p2c nonce {seq}"), 12)
            };
            let plain = fixed(&format!("{name} frame {dir} {seq}"), 17 + seq as usize);
            let aad = cbor(vec![t("MACULA-E2E-STREAM-AAD-V1"), t(ftype), b(&request_id), u(seq), u(dir)]);
            let ct = seal(key, &nonce, &aad, &plain);
            frames.push(json!({
                "direction": dir, "seq": seq, "frame_type": ftype,
                "plain": h(&plain), "aad": h(&aad), "nonce": h(&nonce), "ct": h(&ct),
            }));
        }
        v["k_c2p"] = json!(h(k_c2p));
        v["k_p2c"] = json!(h(k_p2c));
        v["frames"] = json!(frames);
    }
    v
}

fn event_vector() -> Json {
    let k_g = fixed("event k_g", 32);
    let publisher = fixed("event publisher", 32);
    let realm = fixed("event realm", 32);
    let topic = "acme/news/weather";
    let seq: u64 = 1_790_000_000_123;
    let published_at: u64 = 1_790_000_000_456;
    let prk_g = extract(b"MACULA-E2E-EVENT-V1", &k_g);
    let k_pub = expand(&prk_g, &cbor(vec![t("MACULA-E2E-EVENT-V1"), b(&publisher)]), 32);
    let nonce = fixed("event nonce", 12);
    let plain = fixed("event payload", 29);
    let aad = cbor(vec![t("MACULA-E2E-EVENT-AAD-V1"), b(&realm), t(topic), b(&publisher), u(seq), u(published_at)]);
    let ct = seal(&k_pub, &nonce, &aad, &plain);
    json!({
        "k_g": h(&k_g), "publisher": h(&publisher), "prk_g": h(&prk_g),
        "k_pub": h(&k_pub), "realm": h(&realm), "topic": topic, "seq": seq, "published_at": published_at,
        "plain": h(&plain), "aad": h(&aad), "nonce": h(&nonce), "ct": h(&ct),
    })
}

/// A pq_hybrid kem_ct whose ephemeral point makes the recipient's ECDH output
/// all zeros: P-384 has points with x = 0, and E = d^-1 * (0, sqrt b) lands d*E on
/// one (found by macula-go). A recipient must refuse it.
fn zero_ecdh_refusal(hybrid_call: &Json, hybrid: &Json) -> Json {
    let d = hex::decode("00001234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890ab").unwrap();
    let e = hex::decode("04ebdd30df769d32acd5bff007110a624531892fdfb3210020ac0170860ff2266f6795e8ce7e98e0dcbd1d88364e4a0cdac0d941ef41aa7c4a7da63d248ee8b6be1ad4d3a8d0f7e6c8721e209797e522a8c17e82878aadf61d6dbb0dba05341754").unwrap();
    let sk = SecretKey::from_slice(&d).expect("scalar");
    let point = PublicKey::from_sec1_bytes(&e).expect("on the curve");
    let shared = ecdh::diffie_hellman(sk.to_nonzero_scalar(), point.as_affine());
    assert!(shared.raw_secret_bytes().iter().all(|b| *b == 0), "the refusal vector must reach a zero ECDH output");
    let mlkem_ct = hex::decode(hybrid_call["mlkem_ct"].as_str().unwrap()).unwrap();
    json!({
        "why": "the ephemeral point makes the recipient's P-384 ECDH output 48 zero bytes",
        "profile": "pq_hybrid",
        "mlkem_seed": hybrid["mlkem_seed"],
        "mlkem_dk": hybrid["mlkem_dk"],
        "p384_priv": h(&d),
        "key_as_carried": hybrid["key_as_carried"],
        "kem_ct": h(&[mlkem_ct.as_slice(), e.as_slice()].concat()),
        "expect": "sealed_refused",
    })
}

fn main() {
    let pure = recipient("pq_pure");
    let hybrid = recipient("pq_hybrid");
    let hybrid_call = call_vector("pq_hybrid", &hybrid, "call");
    let refusals = vec![zero_ecdh_refusal(&hybrid_call, &hybrid.json)];
    let doc = json!({
        "scheme": 1,
        "spec": "test/vectors/E2E_SEAL_V1.md",
        "generator": "scripts/e2e_seal_vectors",
        "recipients": {"pq_pure": pure.json, "pq_hybrid": hybrid.json},
        "calls": [
            call_vector("pq_pure", &pure, "call"),
            hybrid_call,
            call_vector("pq_hybrid", &hybrid, "stream_open"),
            call_vector("pq_pure", &pure, "stream_open"),
        ],
        "events": [event_vector()],
        "refusals": refusals,
    });
    println!("{}", serde_json::to_string_pretty(&doc).expect("json"));
}
