use std.os.mod
use std.core.test
use std.core.core
use std.strings.str

print("Testing OS Mod...")

def p = pid()
print("PID:")
print(p)
assert(p > 0, "pid > 0")

def pp = ppid()
print("PPID:", pp)
assert(pp > 0, "ppid > 0")

def u = uid()
print("UID:", u)
assert(u >= 0, "uid >= 0")

def g = gid()
; print("GID:", g)
; assert(g >= 0, "gid >= 0")

; env
def path = env("PATH")
print("PATH:", path)
assert(str_len(path) > 0, "env PATH")

def e = environ()
assert(type(e) == "list", "environ list")
assert(len(e) > 0, "environ len")

print("✓ std.os.mod passed")
