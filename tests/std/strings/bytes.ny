use std.strings.bytes
use std.core.test
use std.core.core
use std.strings.str

print("Testing Strings Bytes...")

def b = bytes_from_str("hello")
assert(bytes_len(b) == 5, "bytes_len")
assert(bget(b, 0) == 104, "bget 'h'")
bset(b, 0, 97)
assert(bget(b, 0) == 97, "bset 'a'")
assert(eq(b, "aello"), "cmp bytes") ; it's a string underneath?

def sub = bslice(b, 1, 3)
assert(eq(sub, "el"), "bslice")

def h = hex_encode(bytes_from_str("abc"))
assert(eq(h, "616263"), "hex_encode")

def decoded = hex_decode("616263")
assert(eq(decoded, "abc"), "hex_decode")

print("✓ std.strings.bytes passed")
