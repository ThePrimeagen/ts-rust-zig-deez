/**
 * Hash map structures based on the awesome book
 * "Writing an Interpreter in Go" by Thorsten Ball.
 *
 * Authors: ToraKuma42
 * License: MIT
 * Version: 0.0.1
 */

import std.digest.crc : crc64ECMAOf;

/// Wrapper around 8 byte buffer which can be read as an unsigned long number
union ULongBytes {
    ubyte[8] buffer;
    ulong value;
}

/// Value type tags for hash key
enum HashType : ubyte {
    Unknown,
    Int,
    String,
    Boolean
}

/// Strongly typed structure for hash key
struct HashKey {
    long value;
    HashType type;
}

/// Generate hash key for string value
HashKey hashGen(string value)
{
    static ULongBytes hash;
    hash.buffer = crc64ECMAOf(value);
    return HashKey(hash.value, HashType.String);
}
