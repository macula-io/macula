#!/usr/bin/env bash
# Derive the local gate's steps from the gating workflows (macula#163).
#
#   scripts/ci_gate_plan.sh [--repo DIR] --work DIR
#
# Writes DIR/plan.tsv and one shell script per runnable step, and prints one line per job: whether it runs here or why
# it cannot. Exits 0 when the plan is complete, 1 when the gate and CI have drifted apart, and 3 when a workflow it
# must read could not be read at all.
#
# WHY DERIVED. A hand-copied gate goes out of step with CI in silence, and the gap is found by a red run after the work
# has been handed over. This reads the workflows, so a step CI gains is a step here gains, and a job nobody has ruled on
# is a refusal rather than a quiet skip.
#
# THE RULINGS, in one place. A job either runs inside the pinned image (LOCAL_JOBS) or is listed in RULED_JOBS with the
# reason a member is told. A job in neither is DRIFT: a new job is unknown until somebody decides whether it can run
# locally, and that decision belongs in a commit, not in a silent default. The same goes for a workflow file: one that
# gates a push and is in neither RULED_WORKFLOWS nor the plan is DRIFT, so a new workflow cannot be added and quietly
# never run here.
set -uo pipefail

EXTRACTOR="scripts/ci_gate_steps.awk"

# Jobs whose steps run inside the pinned build image, as "<workflow file>:<job>". The job that calls a reusable
# workflow is given the name of its workflow file; its steps come from the reusable file, fetched at the pinned sha.
# The job that calls the reusable workflow is named erlang-blocks in docs.yml; its steps carry the reusable
# workflow's own job name, snippets. All three are here so the caller and the callee are recognised.
LOCAL_JOBS="test.yml:test docs.yml:erlang-blocks docs.yml:snippets"

# Jobs that cannot run locally, and what a member is told instead of running them.
RULED_JOBS="rust-lint:a plain runner with cargo clippy; the pinned OTP image's Rust carries no clippy component
advisories-erlang:a reusable workflow against the advisory network and a GitHub action, not the build image
advisories-rust:a reusable workflow against the advisory network and a GitHub action, not the build image"

# Workflow files that gate a push and are not in the plan, with the reason a member is told. `publish-hex.yml` is
# tag-only and calls test.yml and docs.yml, which are in the plan; `build-docs-pdf.yml` runs on a push to main but
# builds a PDF in a LaTeX image the gate does not carry. Naming them is the point: a green gate must not read as more
# than it is.
RULED_WORKFLOWS="build-docs-pdf.yml:builds the PDFs on a push to main, in a LaTeX image this gate does not run
publish-hex.yml:runs on a release tag only; its test.yml and docs.yml are in this plan"

usage() { echo "usage: scripts/ci_gate_plan.sh [--repo DIR] --work DIR" >&2; exit 2; }

REPO_DIR="."
WORK=""
while [ "$#" -gt 0 ]; do
    case "$1" in
        --repo) REPO_DIR="${2:?--repo needs a directory}"; shift 2 ;;
        --work) WORK="${2:?--work needs a directory}"; shift 2 ;;
        *) usage ;;
    esac
done
[ -n "$WORK" ] || usage
[ -d "$REPO_DIR/.github/workflows" ] || { echo "PLAN: no .github/workflows under $REPO_DIR" >&2; exit 2; }

REPO_DIR="$(cd "$REPO_DIR" && pwd)"
mkdir -p "$WORK" || exit 2
WORK="$(cd "$WORK" && pwd)"
PLAN="$WORK/plan.tsv"
: > "$PLAN"

DRIFT=()
CANNOT=()

ruling() {  # ruling <job> -> the reason it cannot run here, or nothing
    printf '%s\n' "$RULED_JOBS" | awk -F: -v job="$1" '$1 == job { sub(/^[^:]*:/, ""); print; found = 1 } END { exit found ? 0 : 1 }'
}

workflow_ruling() {  # workflow_ruling <file name> -> the reason it is not in the plan, or nothing
    printf '%s\n' "$RULED_WORKFLOWS" | awk -F: -v f="$1" '$1 == f { sub(/^[^:]*:/, ""); print; found = 1 } END { exit found ? 0 : 1 }'
}

is_local() {  # is_local <workflow file> <job>
    case " $LOCAL_JOBS " in *" $1:$2 "*) return 0 ;; *) return 1 ;; esac
}

STEP_FILES=()

extract() {  # extract <workflow file> <label> <inputs> <dir>
    # One directory per file read, because ci_gate_steps.awk numbers its scripts from 1 every time it is handed a file.
    # Sharing one directory let docs.yml's first three steps overwrite test.yml's, and the gate then ran the docs check
    # three times: steps 1-3 passed for the wrong reason and the docs steps passed having already been run. The check on
    # the assembled plan below is what makes that a refusal rather than a thing to notice in a log.
    mkdir -p "$WORK/$4"
    awk -f "$EXTRACTOR" -v out="$WORK/$4" -v inputs="$3" < "$1" | sed "s/^STEP /STEP\t$2\t/"
}

# Every workflow file, so one that gates a push and is in nobody's plan is DRIFT rather than invisible. Both spellings
# are read: GitHub accepts `.yaml` as readily as `.yml`, and a file the glob does not reach is a workflow nobody is
# watching.
for f in "$REPO_DIR"/.github/workflows/*.yml "$REPO_DIR"/.github/workflows/*.yaml; do
    [ -f "$f" ] || continue
    base="$(basename "$f")"
    case "$base" in
        test.yml|docs.yml) ;;
        *) if ! reason=$(workflow_ruling "$base"); then
               DRIFT+=("unruled-workflow: $base is in .github/workflows and no ruling says whether it gates a push or what the gate does about it")
           else
               CANNOT+=("$base — $reason")
           fi ;;
    esac
done

# Every workflow that gates a push, in the order a member would meet them.
TEST_WORKFLOW="$REPO_DIR/.github/workflows/test.yml"
DOCS_WORKFLOW="$REPO_DIR/.github/workflows/docs.yml"
for f in "$TEST_WORKFLOW" "$DOCS_WORKFLOW"; do
    [ -f "$f" ] || { echo "PLAN: missing $f" >&2; exit 2; }
done

# The matrix checks the gating workflow declares, so a check whose steps all skipped can be refused rather than
# passing having run nothing. Both spellings are read - the flow list `check: [a, b]` and a block list - and quotes are
# stripped, because an entry the parser cannot see is a check nobody is watching.
MATRIX_CHECKS=$(awk '
    /^[[:space:]]*check:[[:space:]]*\[/ {
        line = $0; sub(/^.*check:[[:space:]]*\[/, "", line); sub(/\].*$/, "", line)
        n = split(line, parts, ",")
        for (i = 1; i <= n; i++) { gsub(/[[:space:]"\047]/, "", parts[i]); if (parts[i] != "") print parts[i] }
        next
    }
    /^[[:space:]]*check:[[:space:]]*$/ { inlist = 1; next }
    inlist && /^[[:space:]]*-[[:space:]]*/ {
        v = $0; sub(/^[[:space:]]*-[[:space:]]*/, "", v); gsub(/[[:space:]"\047]/, "", v)
        if (v != "") print v
        next
    }
    inlist { inlist = 0 }
' "$TEST_WORKFLOW" | tr '\n' ' ')
[ -n "${MATRIX_CHECKS// /}" ] || \
    DRIFT+=("no-matrix-checks: test.yml declares no matrix check list the gate can read, so a check whose steps all skipped cannot be seen")

# test.yml: the matrix job's steps, and the jobs that cannot run here.
TEST_ROWS=$(extract "$TEST_WORKFLOW" "test.yml" "" "test.yml")
printf '%s\n' "$TEST_ROWS" > "$WORK/test.rows"

while IFS=$'\t' read -r kind job rest; do
    case "$kind" in
        DRIFT) DRIFT+=("$job${rest:+ $rest}") ;;
        JOB)
            is_local "test.yml" "$job" && continue
            if reason=$(ruling "$job"); then
                CANNOT+=("$job — $reason")
            else
                DRIFT+=("unknown-job: $job in test.yml — decide whether it can run locally, and rule it in scripts/ci_gate_plan.sh")
            fi
            ;;
    esac
done < "$WORK/test.rows"

# docs.yml itself: its own jobs, and the one reusable call it makes. Its jobs are ruled like test.yml's, so a second job
# added there is DRIFT rather than skipped; a second `uses:` is refused because the plan would silently run only the
# first; and a `with:` on the caller is refused because the gate applies the callee's declared defaults, not the
# caller's overrides, and a gate that quietly runs the wrong inputs is worse than one that says it cannot.
DOCS_OWN=$(extract "$DOCS_WORKFLOW" "docs.yml" "" "docs-own")
printf '%s\n' "$DOCS_OWN" > "$WORK/docs-own.rows"
while IFS=$'\t' read -r kind job rest; do
    case "$kind" in
        DRIFT) DRIFT+=("$job${rest:+ $rest}") ;;
        JOB) is_local "docs.yml" "$job" || DRIFT+=("unknown-job: $job in docs.yml — decide whether it can run locally, and rule it in scripts/ci_gate_plan.sh") ;;
    esac
done < "$WORK/docs-own.rows"

USES_ALL=$(grep -c '^[[:space:]]*uses:' "$DOCS_WORKFLOW")
[ "$USES_ALL" -le 1 ] || DRIFT+=("multiple-uses: docs.yml calls $USES_ALL reusable workflows; the plan follows one, so the rest would never run here")
grep -q '^    with:' "$DOCS_WORKFLOW" && \
    DRIFT+=("unapplied-with: docs.yml passes inputs to the reusable workflow; this gate runs the callee's declared defaults and would not apply them")

# docs.yml: a job that calls a reusable workflow. Follow it, at the sha the calling workflow pins, and run those steps
# from the text CI runs rather than from a copy.
DOC_ROWS=""
USES=$(sed -n 's/^[[:space:]]*uses:[[:space:]]*//p' "$DOCS_WORKFLOW" | head -n 1)
if [ -n "$USES" ]; then
    case "$USES" in
        *@[0-9a-f]*)
            REF_SHA="${USES##*@}"
            FILE_PATH="${USES%@*}"
            ORG="${FILE_PATH%%/*}"
            REST="${FILE_PATH#*/}"
            REPO_NAME="${REST%%/*}"
            FILE_IN_REPO="${REST#*/}"
            ORG_REPO="${ORG}/${REPO_NAME}"
            if [ "${#REF_SHA}" -ne 40 ]; then
                DRIFT+=("unpinned: docs.yml calls $USES, which is not pinned by a full sha")
            else
                URL="https://raw.githubusercontent.com/${ORG_REPO}/${REF_SHA}/${FILE_IN_REPO}"
                FETCHED="$WORK/reusable.yml"
                if ! curl -fsS --max-time 30 -o "$FETCHED" "$URL" || [ ! -s "$FETCHED" ]; then
                    echo "PLAN: cannot fetch $URL — the gate cannot run what it cannot read" >&2
                    exit 3
                fi
                # The inputs the caller does not override: their defaults in the reusable workflow's workflow_call block.
                INPUTS=$(awk '
                    /^    inputs:[ ]*$/ { ininputs = 1; next }
                    ininputs && /^      [a-z-]+:[ ]*$/ { name = $0; sub(/^ +/, "", name); sub(/:.*/, "", name); next }
                    ininputs && /^        default:[ ]*/ { v = $0; sub(/^ *default:[ ]*/, "", v); gsub(/["\047]/, "", v); out = out (out == "" ? "" : " ") name "=" v }
                    END { print out }
                ' "$FETCHED")
                DOC_ROWS=$(extract "$FETCHED" "docs.yml" "$INPUTS" "docs.yml")
                printf '%s\n' "$DOC_ROWS" > "$WORK/docs.rows"
                while IFS=$'\t' read -r kind job rest; do
                    case "$kind" in
                        DRIFT) DRIFT+=("$job${rest:+ $rest}") ;;
                        JOB) is_local "docs.yml" "$job" || DRIFT+=("unknown-job: $job in $FILE_IN_REPO — decide whether it can run locally") ;;
                    esac
                done < "$WORK/docs.rows"
            fi
            ;;
        *)
            DRIFT+=("unpinned: docs.yml calls $USES, which is not pinned by a full sha")
            ;;
    esac
else
    DRIFT+=("no-call: docs.yml has no reusable workflow to follow")
fi

# Assemble the plan in order: the gating workflow, then the docs workflow.
order=0
: > "$PLAN"
for rows in "$WORK/test.rows" "$WORK/docs.rows"; do
    [ -f "$rows" ] || continue
    while IFS=$'\t' read -r kind _label _order job name check status reason script; do
        [ "$kind" = "STEP" ] || continue
        case "$status" in
            run)
                order=$((order + 1))
                if ! is_local "test.yml" "$job" && ! is_local "docs.yml" "$job"; then
                    reason="$(ruling "$job" || echo 'unruled')"
                    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$order" "$job" "$name" "$check" "cannot" "$reason" "-" >> "$PLAN"
                    continue
                fi
                if grep -q '@UNRESOLVED@' "$script" 2>/dev/null; then
                    DRIFT+=("unresolved-expression: $name uses \${{ ... }} the gate cannot resolve — an input with no default, or a context such as matrix, env or github")
                fi
                printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$order" "$job" "$name" "$check" "run" "-" "$script" >> "$PLAN"
                STEP_FILES+=("$script")
                ;;
            failure-path)
                printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$order" "$job" "$name" "$check" "skip" "$reason" "-" >> "$PLAN"
                ;;
            *)
                printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$order" "$job" "$name" "$check" "cannot" "unruled" "-" >> "$PLAN"
                ;;
        esac
    done < "$rows"
done

# A matrix check with no step would be green having run nothing.
for check in $MATRIX_CHECKS; do
    if ! awk -F'\t' -v c="$check" '$5 == "run" && $4 == c { found = 1 } END { exit found ? 0 : 1 }' "$PLAN"; then
        DRIFT+=("no-step: matrix check '$check' in test.yml has no runnable step")
    fi
done

# Two rows pointing at one script is one step silently standing in for another: the second write wins and both rows run
# its text. This is not hypothetical - it is what docs.yml's steps did to test.yml's before they were given a directory
# each, and the gate reported those steps green.
SHARED=$(awk -F'\t' '$5 == "run" { print $7 }' "$PLAN" | sort | uniq -d | tr '\n' ' ')
[ -z "$SHARED" ] || DRIFT+=("shared-step-script: two steps of the plan point at one script, so one runs the other's text: $SHARED")

# A ruling for a job the workflows no longer have is a list nobody is reading.
printf '%s\n' "$RULED_JOBS" | while IFS=: read -r job _reason; do
    [ -n "$job" ] || continue
    grep -q "^JOB	$job\$" "$WORK/test.rows" || echo "STALE $job"
done > "$WORK/stale.txt"
while read -r _ stale_job; do
    [ -n "${stale_job:-}" ] || continue
    DRIFT+=("stale-ruling: $stale_job is ruled in scripts/ci_gate_plan.sh but no workflow has that job")
done < "$WORK/stale.txt"

# A workflow ruling for a file that is gone is the same list nobody is reading.
printf '%s\n' "$RULED_WORKFLOWS" | while IFS=: read -r f _reason; do
    [ -n "$f" ] || continue
    [ -f "$REPO_DIR/.github/workflows/$f" ] || echo "STALE $f"
done > "$WORK/stale-wf.txt"
while read -r _ stale_wf; do
    [ -n "${stale_wf:-}" ] || continue
    DRIFT+=("stale-workflow-ruling: $stale_wf is ruled in scripts/ci_gate_plan.sh but .github/workflows has no such file")
done < "$WORK/stale-wf.txt"

for line in "${CANNOT[@]+"${CANNOT[@]}"}"; do echo "NOT-RUN: $line"; done
if [ "${#DRIFT[@]}" -gt 0 ]; then
    for line in "${DRIFT[@]}"; do echo "DRIFT: $line"; done
    exit 1
fi

echo "PLAN: ${#STEP_FILES[@]} step(s) derived from .github/workflows, plan at $PLAN"
exit 0
