//! QUIC application error codes that macula sends when it resets or stops a
//! stream. The Erlang side defines the same table, with the same meanings,
//! in include/macula_quic_error_codes.hrl; a new code goes into both.

/// The sender cancelled the stream: a content transfer cancel, or a stream
/// open cancelled after the peer allowed it.
pub const CANCELLED: u32 = 0;

/// The stream closed, but its queued data could not be written within the
/// linger bound.
pub const LINGER_EXPIRED: u32 = 1;
