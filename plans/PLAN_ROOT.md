# macula plans

Plans, designs and research for the `macula` SDK. The master index of every Macula plan is
`macula-architecture/plans/PLAN_MACULA_ROOT.md`.

## Active

| Document | Description | Status |
|---|---|---|
| [PLAN_POST_QUANTUM_SECURITY.md][pq] | Post-quantum security: profiles, key model, decisions, stages | Planning |
| [PLAN_POST_QUANTUM_SECURITY_PART1.md][pq1] | Part 1: verified facts per stack and the Stage 0 checks | Planning |
| [PLAN_POST_QUANTUM_SECURITY_PART2.md][pq2] | Part 2: work packages per stage and files per repository | Planning |
| [PLAN_POST_QUANTUM_SECURITY_DECISIONS.md][pqd] | Decisions: the full text of each decision | Planning |
| [DESIGN_PQ_HANDSHAKE_FRAMES.md][frames] | Handshake frames, bindings and status statements, byte for byte | Agreed |
| [DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md][signed] | Signed records and frames after the handshake | Agreed |
| [DESIGN_PQ_DHT_SLOTS_AND_BUDGET.md][slots] | DHT slot bounds, slot admission and the verification budget | Agreed |

## Research

| Document | Description | Status |
|---|---|---|
| [EXPLORATION_POST_QUANTUM.md][explore] | Post-quantum feasibility per stack, before the plan | Investigation |
| [RESEARCH_SDK_STATION_SELECTION_SURVEY.md][stations] | How each SDK chooses a station | Research complete |
| [PLAN_MILITARY_GRADE.md][military] | Belgian, EU and NATO bodies and texts to start from for Defence software | Reference |

## Complete

| Document | Description | Status |
|---|---|---|
| [PLAN_PER_STREAM_QUIC_ISOLATION.md][isolation] | Per-stream QUIC isolation | Phases 1 and 2 complete |
| [PLAN_PUSH_UPLOAD.md][push] | Push-initiated content transfer | Shipped in 9.13.0 |

[pq]: PLAN_POST_QUANTUM_SECURITY.md
[pq1]: PLAN_POST_QUANTUM_SECURITY_PART1.md
[pq2]: PLAN_POST_QUANTUM_SECURITY_PART2.md
[pqd]: PLAN_POST_QUANTUM_SECURITY_DECISIONS.md
[frames]: DESIGN_PQ_HANDSHAKE_FRAMES.md
[signed]: DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md
[slots]: DESIGN_PQ_DHT_SLOTS_AND_BUDGET.md
[explore]: EXPLORATION_POST_QUANTUM.md
[stations]: RESEARCH_SDK_STATION_SELECTION_SURVEY.md
[military]: PLAN_MILITARY_GRADE.md
[isolation]: PLAN_PER_STREAM_QUIC_ISOLATION.md
[push]: PLAN_PUSH_UPLOAD.md
