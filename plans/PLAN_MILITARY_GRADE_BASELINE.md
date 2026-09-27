# Military grade: technical baseline and gap register

**This exists so Macula mesh and realm reach "technically military grade, or better, within our means", measured against named requirements rather than a slogan.**

**Status:** Yardstick only. The living register moved on 2026-09-27 (see section 2). Classification: BUILD (a register, no claims). **Created:** 2026-09-25. **Owner:** Pluto.
**Certification is out of scope.** Nothing here says Macula meets, complies with or is certified against any rule
below. Public wording stays under D11 in [PLAN_POST_QUANTUM_SECURITY_DECISIONS.md][d11]. The bodies and processes
for the Belgian Defence (NVO/ANS CIS approval, CCB/CyFun, BOSA, e-Procurement, NATO) are in
[PLAN_MILITARY_GRADE.md][mg]; this document does not repeat them.

**Checked against:** `macula` origin/main bf1d732e (v12.5.0), `macula-station` release v0.6.2 (what the fleet runs)
and main 2cf0e8c, `macula-realm` origin/main efefaee, the wire measurement
`~/.claude/sessions/PQKX_MEASUREMENT_2026-09-24.md`, and the post review
`~/.claude/sessions/REVIEW_2026-09-25_architecting_survivability.md`. Public text derived from this register:
`macula-comm-docs/posts/architecting-survivability.v2.md`.
**Updated 2026-09-26:** rows 1, 3 and 5 against `macula-station` tags v0.6.3 to v0.6.7, the fleet on v0.6.7 since
15:42Z, and cipher-suite wire checks on all six stations and a macula-go client; the rest stand as checked above.

---

## 1. The yardstick

| Source | What it gives us | Where |
|---|---|---|
| NSA CNSA 2.0 advisory and FAQ | algorithm list (ML-KEM-1024, ML-DSA-87, AES-256, SHA-384/512) | summarised with quotes in [PLAN_POST_QUANTUM_SECURITY.md][pq] "Requirements that apply to both profiles" |
| BSI TR-02102-1 and TR-02102-2 | hybrid use, TLS groups (TR-02102-2 section 3.4.2), hybrid signature construction (TR-02102-1 section 5.3.4) | [PLAN_POST_QUANTUM_SECURITY.md][pq], D11 |
| EU Cyber Resilience Act, Regulation (EU) 2024/2847, Annex I | essential requirements for a product with digital elements: Part I (2)(d) access control, (e) confidentiality in transit, (f) integrity, (h) availability and DoS resilience, (j) attack surface, (l) security logging; Part II (1) SBOM, (3) security testing, (7) secure update distribution | EUR-Lex, text checked 2026-09-25 |
| NIS2, Directive (EU) 2022/2555, Article 21(2) | measures an essential entity must take: (d) supply chain security, (e) secure development and vulnerability handling, (h) cryptography policy | EUR-Lex, text checked 2026-09-25 |
| Military CIS properties: traffic-flow confidentiality, node capture, key management | expected by a national CIS accreditation (NVO/ANS in Belgium); the requirement texts are not public | [PLAN_MILITARY_GRADE.md][mg] section 2.1; the concrete tender sets the bar (section 1) |

A property whose requirement text is not public is marked **(source: accreditation, not public)**. Its row states
what a CIS accreditation typically examines, and the tender or the accreditor sets the exact bar.

## 2. The register

The living register of features, gaps, threats and evidence moved on 2026-09-27 to
`macula-io/macula-architecture/security/features.yaml` (private repository; published as a PDF). It is generated,
checked in CI and updated in the same commit range as any change that moves a feature, so it supersedes this file's
former sections 2 to 4 (the ranked gap register, its reading and its next steps). Row numbers cited elsewhere, such as
#14 end-to-end confidentiality and #18 traffic-flow confidentiality, refer to that former list, kept in git history
at 92137b94.

[mg]: PLAN_MILITARY_GRADE.md
[pq]: PLAN_POST_QUANTUM_SECURITY.md
[d11]: PLAN_POST_QUANTUM_SECURITY_DECISIONS.md#d11-what-public-text-may-claim
