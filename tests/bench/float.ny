use std.math.float
use std.math.time
use std.core.core
use std.io.fmt

print("Benchmarking Float...")

def start = ticks()

def x = 1.0
def y = 1.000001
def i = 0
while(i < 1000000){
    x = x * y
    i = i + 1
}

def end = ticks()
def dur = end - start
print("Float 1M muls took (ns): ", dur)
print("Result: ", x)
