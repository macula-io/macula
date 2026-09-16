//! QUIC application error codes that macula sends when it resets or stops a
//! stream, or closes a connection. The Erlang side defines the same table,
//! with the same meanings, in include/macula_quic_error_codes.hrl; a new code
//! goes into both. Some codes are sent only by Erlang code, so the NIF itself
//! never names them.
#![allow(dead_code)]

/// The sender cancelled the stream: a content transfer cancel, or a stream
/// open cancelled after the peer allowed it.
pub const CANCELLED: u32 = 0;

/// The stream closed, but its queued data could not be written within the
/// linger bound.
pub const LINGER_EXPIRED: u32 = 1;

/// The stream was refused before it was served: its first frame was not a
/// STREAM_OPEN signed by its caller.
pub const REFUSED: u32 = 2;

/// An established stream was aborted because a frame on it did not decode.
pub const STREAM_PROTOCOL_ERROR: u32 = 3;

/// The node had no room: a connection closed because the station had no
/// handshake slot free, or a relayed stream reset because its reader did not
/// take data in time. The peer may try again later, or another station.
pub const REFUSED_BUSY: u32 = 4;
