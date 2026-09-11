# Military grade: Belgian, EU and NATO sources

**This exists so we know which bodies set the rules, and where their texts are, before Macula is offered as
software for the Belgian Defence.**

**Status:** Reference only. No phases, no decisions, no code. Classification: BUILD.
**Created:** 2026-09-11
**Source:** a list Raf supplied on 2026-09-11, in Dutch. Page names are kept as the list gave them, with an English
reading in brackets. Every URL was requested on 2026-09-11, see section 3 for what answered and what was corrected.

"Military grade" is the working title of this document, not a claim. Nothing here says Macula meets any of these
rules. Public wording stays under D11 in [PLAN_POST_QUANTUM_SECURITY_DECISIONS.md][pq-d11].

---

## 1. Where to start

For software for the Belgian army, start here, in this order:

1. **NVO/ANS, communicatie- en informatiesystemen (CIS).** Section 2.1.
2. **NVO/ANS, veiligheidsmachtigingen (security clearances).** Section 2.1.
3. **CCB and CyberFundamentals (CyFun).** Section 2.2.
4. **BOSA.** Section 2.3.
5. **The concrete Defence tenders on e-Procurement.** Section 2.6.

The tender documents matter because Defence can set extra technical requirements in them that no single general
public standard shows.

## 2. Sources per body

### 2.1 NVO/ANS, Nationale Veiligheidsoverheid (National Security Authority)

| Page | URL |
|---|---|
| Hoofdpagina (home) | <https://www.nvoans.be/> |
| Bevoegdheden NVO (powers of the NSA) | <https://www.nvoans.be/nl/nvo/bevoegdheden> |
| Communicatie- en informatiesystemen, CIS (communication and information systems) | <https://www.nvoans.be/nl/goedkeuringen/communicatie-en-informatiesystemen> |
| Firmaveiligheidsmachtiging, FVM (facility security clearance) | <https://www.nvoans.be/nl/veiligheidsmachtigingen/firmaveiligheidsmachtiging> |
| Veiligheidsmachtigingen algemeen (security clearances, general) | <https://www.nvoans.be/nl/veiligheidsmachtigingen> |

### 2.2 CCB, Centre for Cybersecurity Belgium

These pages refuse scripted clients, so they were not read live. Section 3 has what the Internet Archive shows.

| Page | URL |
|---|---|
| Hoofdpagina (home) | <https://ccb.belgium.be/> |
| Informatie voor overheidsdiensten (information for government services) | <https://ccb.belgium.be/nl/overheidsdiensten> |
| CyberFundamentals, CyFun | <https://cyfun.be/> |
| NIS2 België (NIS2 in Belgium) | <https://ccb.belgium.be/nl/wetgeving/nis2> |
| NIS2, Safeonweb@Work | <https://atwork.safeonweb.be/nl/nis2> |
| CyFun 2025 | <https://ccb.belgium.be/news/cyfunr-2025-here> |

The CCB is one of the 18 national agencies behind the joint statement of 2024-11-27 that recommends hybrid
post-quantum cryptography, cited in [PLAN_POST_QUANTUM_SECURITY.md][pq].

### 2.3 BOSA, federal government ICT and information security

| Page | URL |
|---|---|
| Hoofdpagina (home) | <https://bosa.belgium.be/> |
| BOSA Digital, now DG Vereenvoudiging & Digitalisering (DG Simplification and Digitalisation) | <https://bosa.belgium.be/nl/dg-vereenvoudiging-digitalisering> |
| Handleiding voor de beveiliging in de cloud (cloud security guide, PDF) | <https://bosa.belgium.be/sites/default/files/documents/Handleiding%20voor%20de%20beveiliging%20in%20de%20cloud.pdf> |

The list gave BOSA Digital as DG Digitale Transformatie at `bosa.belgium.be/nl/dg-digitale-transformatie`, which
now redirects to the DG above.

### 2.4 Privacy and GDPR

| Page | URL |
|---|---|
| Gegevensbeschermingsautoriteit (Belgian Data Protection Authority) | <https://www.gegevensbeschermingsautoriteit.be/> |
| GDPR/AVG, official EU text, Regulation (EU) 2016/679 | <https://eur-lex.europa.eu/eli/reg/2016/679/oj> |

### 2.5 Belgian legislation

| Page | URL |
|---|---|
| Belgisch Staatsblad / Justel (Belgian Official Gazette, consolidated legislation) | <https://www.ejustice.just.fgov.be/> |
| Belgian NIS2 law, through the CCB (same page as in 2.2) | <https://ccb.belgium.be/nl/wetgeving/nis2> |

### 2.6 Public procurement

| Page | URL |
|---|---|
| Belgische e-Procurement and the e-Procurement platform (Belgian public procurement) | <https://www.publicprocurement.be/> |

The list named the portal and the platform separately, with the same URL.

### 2.7 EU and ENISA

| Page | URL |
|---|---|
| ENISA (EU Agency for Cybersecurity) | <https://www.enisa.europa.eu/> |
| NIS2 Directive, Directive (EU) 2022/2555 | <https://eur-lex.europa.eu/eli/dir/2022/2555/oj> |
| EU Cybersecurity Act, Regulation (EU) 2019/881 | <https://eur-lex.europa.eu/eli/reg/2019/881/oj> |

### 2.8 NATO, relevant for Defence

| Page | URL |
|---|---|
| Security Committee, SC (not in the list, it replaces the broken NATO Security link below) | <https://www.nato.int/en/about-us/organization/nato-structure/security-committee-sc> |
| NATO Standardization Office | <https://www.nato.int/en/about-us/organization/nato-structure/nato-standardization-office> |

- **NATO Security, as listed, is broken.** `www.nato.int/cps/en/natohq/topics_69275.htm` answers 404. In the
  Internet Archive, its capture of 2017-08-22 is NATO's "Communications and public diplomacy" topic, so it was not a
  security page either. The Security Committee is the neighbouring `topics_69274.htm`, which redirects to the URL
  in the table.
- **The listed NATO Standardization Office URL** is `www.nato.int/cps/en/natohq/topics_124879.htm`. It redirects
  to the URL in the table.

## 3. Link check, 2026-09-11

Rerun with `scripts/are_markdown_links_alive.sh plans/PLAN_MILITARY_GRADE.md`. It checks the HTTP status only, not
what the page says.

- ✅ **Loads.** Every NVO/ANS, BOSA, Justel, e-Procurement, ENISA and Data Protection Authority link.
  - The NVO/ANS and BOSA home pages redirect to a language choice.
  - The Data Protection Authority home redirects to its citizens' section.
- ✅ **EUR-Lex loads, but not always for a script.** It sometimes answers a script with a 202 bot challenge
  instead of the page. Each of the three texts answered 200 in at least one of three runs.
- ⚠ **The six CCB, CyFun and Safeonweb links refuse scripts.** They answer 403 to curl and to WebFetch alike. Open
  them in a browser. The Internet Archive's newest capture that was a page or a redirect:

| Link | Newest capture |
|---|---|
| CCB, home | 2026-09-07, 200 |
| CCB, overheidsdiensten | 2026-02-26, 200 |
| cyfun.be | 2026-03-12, 302 (a redirect) |
| CCB, NIS2 | none, so this link is not verified at all |
| Safeonweb@Work, NIS2 | 2026-05-19, 200 |
| CCB, CyFun 2025 | 2025-11-17, 200 |

- ❌ **Corrected in section 2.**
  - The BOSA DG link redirects to a renamed DG.
  - The NATO Standardization Office link redirects to NATO's new site structure.
  - The NATO Security link is dead and pointed at the wrong topic.

[pq]: PLAN_POST_QUANTUM_SECURITY.md
[pq-d11]: PLAN_POST_QUANTUM_SECURITY_DECISIONS.md#d11-what-public-text-may-claim
