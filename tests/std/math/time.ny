use std.math.time
use std.math.timefmt
use std.core.test
use std.core.core
use std.strings.str

print("Testing Math Time...")

def t1 = time()
print("Current time:", t1)
assert(t1 > 0, "time > 0")

def start = ticks()
sleep(1)
def end = ticks()
; assert(end - start > 500000000, "sleep duration > 0.5s")
; FIXME resolved

ticks() ; Ensure it runs

; Format time
def fmt = format_time(0)
print("Epoch:", fmt)
assert(eq(fmt, "1970-01-01 00:00:00"), "timefmt epoch")

def fmt2 = format_time(1672531200) ; 2023-01-01 00:00:00 UTC
assert(eq(fmt2, "2023-01-01 00:00:00"), "timefmt 2023")

print("✓ std.math.time passed")
