//! QUIC application error codes that the NIF itself sends when it resets or
//! stops a stream. The full table of every code macula sends, with the same
//! names and meanings, is include/macula_quic_error_codes.hrl.

/// The sender cancelled the stream: a content transfer cancel, or a stream
/// open cancelled after the peer allowed it.
pub const CANCELLED: u32 = 0;

/// The stream closed, but its queued data could not be written within the
/// linger bound.
pub const LINGER_EXPIRED: u32 = 1;
