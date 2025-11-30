#!/bin/bash
# Test P2P Hole Punching between peers
#
# Usage:
#   ./test-hole-punch.sh              - Run all tests
#   ./test-hole-punch.sh fc01 rc01    - Test specific peer pair

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_section() { echo -e "\n${BLUE}=== $1 ===${NC}"; }

# Get peer's internal IP based on container name
get_peer_ip() {
    local peer=$1
    case $peer in
        fc0[1-9]|fc10)
            local num=${peer#fc}
            num=${num#0}  # Remove leading zero
            echo "172.31.1.$((9 + num))"
            ;;
        rc0[1-9]|rc10)
            local num=${peer#rc}
            num=${num#0}
            echo "172.32.1.$((9 + num))"
            ;;
        sy0[1-9]|sy10)
            local num=${peer#sy}
            num=${num#0}
            echo "172.33.1.$((9 + num))"
            ;;
        bootstrap)
            echo "10.100.0.10"
            ;;
        *)
            echo ""
            ;;
    esac
}

# Get peer's Erlang node name
get_node_name() {
    local peer=$1
    local ip=$(get_peer_ip "$peer")
    echo "${peer}@${ip}"
}

# Test direct QUIC connection attempt (without hole punch coordination)
test_direct_connection() {
    local from_peer=$1
    local to_peer=$2
    local to_ip=$(get_peer_ip "$to_peer")
    local from_ip=$(get_peer_ip "$from_peer")
    local from_node=$(get_node_name "$from_peer")

    log_info "Testing direct connection: $from_peer -> $to_peer ($to_ip:4433)"

    # Try to establish a direct QUIC connection
    local result=$(docker exec chatter-${from_peer} erl -noshell \
        -name "test_$$@${from_ip}" \
        -setcookie nat_test_cookie \
        -eval "
            Result = rpc:call('${from_node}', macula_peer_connector, send_message,
                [<<\"${to_ip}:4433\">>, ping, #{<<\"from\">> => <<\"${from_peer}\">>}]),
            io:format('~p', [Result]).
        " -s init stop 2>&1)

    if [[ "$result" == *"ok"* ]]; then
        echo -e "  ${GREEN}✓ Direct connection succeeded${NC}"
        return 0
    else
        echo -e "  ${YELLOW}✗ Direct connection failed: $result${NC}"
        return 1
    fi
}

# Test NAT-aware connection (uses hole punch and relay fallback)
test_nat_aware_connection() {
    local from_peer=$1
    local to_peer=$2
    local from_ip=$(get_peer_ip "$from_peer")
    local to_ip=$(get_peer_ip "$to_peer")
    local from_node=$(get_node_name "$from_peer")

    # Generate pseudo node IDs for testing
    local from_node_id=$(echo -n "$from_peer" | sha256sum | cut -c1-64)
    local to_node_id=$(echo -n "$to_peer" | sha256sum | cut -c1-64)

    log_info "Testing NAT-aware connection: $from_peer -> $to_peer"

    local result=$(docker exec chatter-${from_peer} erl -noshell \
        -name "test_nat_$$@${from_ip}" \
        -setcookie nat_test_cookie \
        -eval "
            FromNodeId = <<16#${from_node_id}:256>>,
            ToNodeId = <<16#${to_node_id}:256>>,
            Opts = #{endpoint => <<\"${to_ip}:4433\">>},
            Result = rpc:call('${from_node}', macula_nat_connector, connect,
                [FromNodeId, ToNodeId, Opts]),
            io:format('~p', [Result]).
        " -s init stop 2>&1)

    if [[ "$result" == *"{ok,"* ]]; then
        echo -e "  ${GREEN}✓ NAT-aware connection succeeded${NC}"
        # Extract connection type
        if [[ "$result" == *"direct"* ]]; then
            echo -e "  ${GREEN}  Strategy: direct${NC}"
        elif [[ "$result" == *"hole_punch"* ]]; then
            echo -e "  ${GREEN}  Strategy: hole_punch${NC}"
        elif [[ "$result" == *"relay"* ]]; then
            echo -e "  ${YELLOW}  Strategy: relay (fallback)${NC}"
        fi
        return 0
    else
        echo -e "  ${RED}✗ NAT-aware connection failed: $result${NC}"
        return 1
    fi
}

# Test hole punch execution directly
test_hole_punch() {
    local from_peer=$1
    local to_peer=$2
    local from_ip=$(get_peer_ip "$from_peer")
    local to_ip=$(get_peer_ip "$to_peer")
    local from_node=$(get_node_name "$from_peer")

    local to_node_id=$(echo -n "$to_peer" | sha256sum | cut -c1-64)

    log_info "Testing hole punch: $from_peer -> $to_peer ($to_ip)"

    local result=$(docker exec chatter-${from_peer} erl -noshell \
        -name "test_hp_$$@${from_ip}" \
        -setcookie nat_test_cookie \
        -eval "
            ToNodeId = <<16#${to_node_id}:256>>,
            Opts = #{
                target_host => <<\"${to_ip}\">>,
                target_ports => [4433],
                session_id => <<\"test_session\">>,
                role => initiator
            },
            Result = rpc:call('${from_node}', macula_hole_punch, execute,
                [ToNodeId, Opts, 5000]),
            io:format('~p', [Result]).
        " -s init stop 2>&1)

    if [[ "$result" == *"{ok,"* ]]; then
        echo -e "  ${GREEN}✓ Hole punch succeeded${NC}"
        return 0
    elif [[ "$result" == *"timeout"* ]]; then
        echo -e "  ${YELLOW}✗ Hole punch timed out (expected for some NAT combinations)${NC}"
        return 1
    else
        echo -e "  ${RED}✗ Hole punch failed: $result${NC}"
        return 1
    fi
}

# Get NAT profile from peer
get_nat_profile() {
    local peer=$1
    local ip=$(get_peer_ip "$peer")
    local node=$(get_node_name "$peer")

    local result=$(docker exec chatter-${peer} erl -noshell \
        -name "test_profile_$$@${ip}" \
        -setcookie nat_test_cookie \
        -eval "
            Result = rpc:call('${node}', macula_nat_detector, get_nat_profile, []),
            io:format('~p', [Result]).
        " -s init stop 2>&1)

    echo "$result"
}

# Run tests for a specific peer pair
test_peer_pair() {
    local from_peer=$1
    local to_peer=$2

    log_section "Testing: $from_peer -> $to_peer"

    # Test 1: Direct connection (likely to fail across NAT)
    test_direct_connection "$from_peer" "$to_peer" || true

    # Test 2: Hole punch
    test_hole_punch "$from_peer" "$to_peer" || true

    # Test 3: NAT-aware connection (should work via relay if hole punch fails)
    test_nat_aware_connection "$from_peer" "$to_peer" || true

    echo ""
}

# Main test suite
run_all_tests() {
    log_section "P2P Hole Punch Test Suite"
    echo "Testing direct connections and hole punching across NAT types"
    echo ""

    # First, check NAT profiles
    log_section "NAT Profiles"
    for peer in fc01 rc01 sy01 bootstrap; do
        log_info "NAT profile for $peer:"
        get_nat_profile "$peer"
        echo ""
    done

    # Test same NAT type (should work easily)
    log_section "Same NAT Type Tests"
    test_peer_pair "fc01" "fc02"   # Full Cone -> Full Cone
    test_peer_pair "rc01" "rc02"   # Restricted -> Restricted
    test_peer_pair "sy01" "sy02"   # Symmetric -> Symmetric

    # Test cross NAT type (harder)
    log_section "Cross NAT Type Tests"
    test_peer_pair "fc01" "rc01"   # Full Cone -> Restricted
    test_peer_pair "fc01" "sy01"   # Full Cone -> Symmetric
    test_peer_pair "rc01" "sy01"   # Restricted -> Symmetric

    # Test to/from bootstrap (public IP, should always work)
    log_section "Public IP Tests"
    test_peer_pair "fc01" "bootstrap"
    test_peer_pair "sy01" "bootstrap"

    log_section "Test Summary"
    echo "Tests complete. Check logs for detailed results."
}

# Main
if [ $# -eq 2 ]; then
    test_peer_pair "$1" "$2"
elif [ $# -eq 0 ]; then
    run_all_tests
else
    echo "Usage: $0 [from_peer to_peer]"
    echo ""
    echo "Examples:"
    echo "  $0              # Run all tests"
    echo "  $0 fc01 rc01    # Test specific pair"
    exit 1
fi
