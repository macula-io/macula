# Extract the steps of a gating workflow, in the workflow's own order (macula#163).
#
# Reads a workflow file on stdin and writes one shell script per runnable step into the directory given by -v out=DIR,
# plus these lines on stdout:
#
#   JOB   <job name>                                    one per job in the file, in order
#   STEP  <order> <job> <name> <check> <status> <reason> <script>
#   DRIFT <what is wrong with this workflow file>
#
# status is run, cannot or failure-path. A `run` script is self-contained: it changes into the step's working directory,
# exports the environment CI gives the step, and then runs the step's `run:` text with the YAML block indentation
# removed, which is the text the shell is handed in CI - heredocs, backslash continuations and all. Nothing here
# rewrites a step.
#
# Whether a step SHOULD run here is not decided in this file: this file reports what each step is and whether it can be
# run at all. The gate's rulings on jobs live in one place, in ci_gate_plan.sh, so an exclusion cannot hide in a parser.
#
# THE SHELL IS CI'S, not a stricter one. A Linux runner hands a step to `bash -e {0}` unless the workflow, the job or
# the step says `shell:`. `shell: bash` is `bash --noprofile --norc -eo pipefail {0}`. Writing `set -euo pipefail` into
# every step made the gate refuse steps CI runs happily - an unset variable, or a pipe whose left side failed - which is
# the same lie in the other direction. A shell this file does not model is reported as DRIFT rather than guessed at.
#
# THE ENVIRONMENT IS CI'S. The workflow's `env:` block comes first, then the job's, then the step's own, each
# overriding the last; the workflow block is the one that matters most, because the pinned toolchain versions live
# there and every script asserts them.
#
# THE WORKFLOW SHAPE this reads is the one these repositories are written in: `jobs:` at column 0, a job at two spaces,
# `steps:` at four, a step item at six, its keys at eight, a block scalar's content below that.
#
# -v inputs="name=value ..." substitutes ${{ inputs.name }}, for reusable workflows. A value the gate cannot resolve -
# an input with no default, or any other `${{ ... }}` context - is left in place and marked @UNRESOLVED@, so the gate
# refuses and names the cause instead of dying in bash with "bad substitution".

function emit(order, status, reason, script,   why) {
    # An empty field between tabs would collapse on the way back in: tab is IFS whitespace, so two tabs read as one
    # separator and every field after it shifts. A reason is therefore always a word, "-" when there is none to give.
    why = (reason == "" ? "-" : reason)
    printf "STEP %s\t%s\t%s\t%s\t%s\t%s\t%s\n", order, job, name, check, status, why, script
}

function drift(msg) { printf "DRIFT\t%s\n", msg }

function strip(s) { sub(/^[ ]+/, "", s); sub(/[ ]+$/, "", s); return s }

function value(line,   v) { v = line; sub(/^[ ]*[a-z-]+:[ ]*/, "", v); return strip(v) }

function substitute(s,   p, k) {
    while (match(s, /\$\{\{[ ]*inputs\.[^ ]+[ ]*\}\}/)) {
        p = substr(s, RSTART, RLENGTH)
        k = p; sub(/^\$\{\{[ ]*inputs\./, "", k); sub(/[ ]*\}\}$/, "", k)
        if (inputval[k] == "") return s "@UNRESOLVED@" p
        s = substr(s, 1, RSTART - 1) inputval[k] substr(s, RSTART + RLENGTH)
    }
    return s
}

# Anything a `${{ ... }}` still stands for is a value CI substitutes and this gate cannot: a matrix entry, another
# workflow's output, the ref, a secret. Left alone it becomes "bad substitution" inside the step, which says nothing
# about the cause, so the script is marked and the plan refuses it by name.
function unresolved(s) { return index(s, "${{") > 0 }

# The shell flags for a step, given the shell CI would give it. Empty means the shell is not modelled here.
function shellflags(s) {
    if (s == "" || s == "default") return "-e"
    if (s == "bash") return "-eo pipefail"
    return ""
}

# A condition that belongs to the failure path: failure() or cancelled() as a bare predicate, not negated. `!cancelled()`
# is the opposite - it says "run even though something before me failed" - and a step gated on an earlier step's outcome
# is run by this gate anyway, because the gate stops at the first refusing step.
function on_failure_path(cond) {
    return cond ~ /(^|[^![:alnum:]_])failure\(\)/ || cond ~ /(^|[^![:alnum:]_])cancelled\(\)/
}

function flush(   path, i, sh, flags, marked) {
    if (stepopen == 0) return
    if (runtext != "") {
        if (on_failure_path(cond)) {
            emit(++order, "failure-path", "if: names failure() or cancelled(): a step for the failure path", "-")
        } else {
            sh = (step_shell != "" ? step_shell : (job_shell != "" ? job_shell : wf_shell))
            flags = shellflags(sh)
            if (flags == "") {
                drift("unmodelled-shell: " job " / " name " declares shell: " sh ", which this gate does not model")
            } else {
                path = sprintf("%s/%04d.sh", out, ++order)
                print "set " flags > path
                print "cd \"${GATE_ROOT:-.}/" workdir "\"" > path
                marked = 0
                for (i = 1; i <= nwfenv; i++) {
                    if (unresolved(wfenv[i])) marked = 1
                    print "export " wfenv[i] > path
                }
                for (i = 1; i <= njobenv; i++) {
                    if (unresolved(jobenv[i])) marked = 1
                    print "export " jobenv[i] > path
                }
                for (i = 1; i <= nenv; i++) {
                    if (unresolved(env[i])) marked = 1
                    print "export " env[i] > path
                }
                if (unresolved(runtext)) marked = 1
                printf "%s", runtext > path
                if (marked) print "# @UNRESOLVED@" > path
                close(path)
                emit(order, "run", "", path)
            }
        }
    }
    stepopen = 0; name = ""; check = "-"; runtext = ""; cond = ""; nenv = 0; workdir = "."; step_shell = ""
}

BEGIN {
    order = 0; stepopen = 0; job = "-"; name = ""; check = "-"; runtext = ""; cond = ""
    nenv = 0; workdir = "."; insteps = 0; inrun = 0; inenv = 0; runindent = 0
    nwfenv = 0; inwfenv = 0; njobenv = 0; injobenv = 0
    wf_shell = ""; job_shell = ""; step_shell = ""
    wdef = 0; wdefrun = 0; jdef = 0; jdefrun = 0
    ninp = split(inputs, pairs, " ")
    for (i = 1; i <= ninp; i++) {
        eq = index(pairs[i], "=")
        if (eq > 0) inputval[substr(pairs[i], 1, eq - 1)] = substr(pairs[i], eq + 1)
    }
}

# A key: value pair of an env block, as NAME=value. The first colon is the separator; anything after it is the value, so
# a value may contain colons of its own.
function envpair(line,   entry) {
    entry = substitute(strip(line))
    sub(/:[ ]*/, "=", entry)
    return entry
}

# The workflow's own env: block, exported into every step. A blank line and a comment line belong to the block and do
# not end it - both are ordinary YAML - and an entry written after one is still an entry. The block ends only at a
# non-comment line indented less than its entries.
/^env:[ ]*$/ { inwfenv = 1; next }
inwfenv && /^  [A-Za-z_][A-Za-z0-9_]*:/ { wfenv[++nwfenv] = envpair($0); next }
inwfenv && /^[[:space:]]*$/ { next }
inwfenv && /^[[:space:]]*#/ { next }
inwfenv && !/^  / { inwfenv = 0 }

# defaults.run.shell at workflow level: column 0 `defaults:` and its two keys, each indented past the last. Blank and
# comment lines belong to it, as they do to an env block.
/^defaults:[ ]*$/ { wdef = 1; next }
wdef && /^  run:[ ]*$/ { wdefrun = 1; next }
wdefrun && /^    shell:[ ]*/ { wf_shell = substitute(value($0)); wdef = 0; wdefrun = 0; next }
wdef && /^[[:space:]]*$/ { next }
wdef && /^[[:space:]]*#/ { next }
wdef && /^[^ ]/ { wdef = 0; wdefrun = 0 }

/^jobs:[ ]*$/ { injobs = 1; next }
/^[A-Za-z_]+:/   { injobs = 0; next }

# A job header: two spaces, a name, a colon.
/^  [A-Za-z0-9_-]+:[ ]*$/ && injobs {
    flush()
    job = $0; sub(/^  /, "", job); sub(/:.*/, "", job)
    print "JOB\t" job
    insteps = 0; inrun = 0; inenv = 0; injobenv = 0; njobenv = 0
    job_shell = ""; jdef = 0; jdefrun = 0
    next
}

# A job's env: block, exported into that job's steps, and its defaults.run.shell. Both end at the first line that is
# not underneath them - a `strategy:`, a `container:`, a `steps:` all close the block, which is why the close rule is
# about the indentation rather than about the three keys that happen to follow today.
injobs && !insteps && /^    env:[ ]*$/ { injobenv = 1; next }
injobenv && /^      [A-Za-z_][A-Za-z0-9_]*:/ { jobenv[++njobenv] = envpair($0); next }
injobenv && /^[[:space:]]*$/ { next }
injobenv && /^[[:space:]]*#/ { next }
injobenv && !/^      / { injobenv = 0 }

injobs && !insteps && /^    defaults:[ ]*$/ { jdef = 1; next }
jdef && /^      run:[ ]*$/ { jdefrun = 1; next }
jdefrun && /^        shell:[ ]*/ { job_shell = substitute(value($0)); jdef = 0; jdefrun = 0; next }
jdef && /^[[:space:]]*$/ { next }
jdef && /^[[:space:]]*#/ { next }
jdef && !/^    / { jdef = 0; jdefrun = 0 }

/^    steps:[ ]*$/ { insteps = 1; injobenv = 0; jdef = 0; jdefrun = 0; next }

insteps && /^      - / {
    flush()
    stepopen = 1
    rest = $0; sub(/^      - /, "", rest)
    if (rest ~ /^name:[ ]/) name = substitute(value(rest))
    inrun = 0; inenv = 0
    next
}

insteps && stepopen && /^        name:[ ]/ { name = substitute(value($0)); next }
insteps && stepopen && /^        if:[ ]/ {
    cond = substitute(value($0))
    if (match(cond, /matrix\.check == '[A-Za-z0-9_-]+'/)) check = substr(cond, RSTART + 17, RLENGTH - 18)
    next
}
insteps && stepopen && /^        working-directory:[ ]/ { workdir = substitute(value($0)); next }
insteps && stepopen && /^        shell:[ ]*/ { step_shell = substitute(value($0)); next }
insteps && stepopen && /^        env:[ ]*$/ { inenv = 1; inrun = 0; next }
insteps && stepopen && /^        run:[ ]*[|>][ ]*$/ { inrun = 1; inenv = 0; runindent = 0; runtext = ""; next }
insteps && stepopen && /^        run:[ ]/ {
    inrun = 0; inenv = 0
    runtext = substitute(value($0)) "\n"
    next
}

insteps && stepopen && inenv && /^          [A-Za-z_][A-Za-z0-9_]*:/ {
    env[++nenv] = envpair($0)
    next
}

insteps && stepopen && inrun {
    if ($0 ~ /^[ ]*$/) { runtext = runtext "\n"; next }
    match($0, /^[ ]*/)
    if (runindent == 0) runindent = RLENGTH
    if (RLENGTH < runindent) { inrun = 0; next }
    runtext = runtext substitute(substr($0, runindent + 1)) "\n"
    next
}

END { flush() }
