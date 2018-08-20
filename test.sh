#!/bin/bash

r9cc="./target/debug/r9cc"

try() {
    expected="$1"
    input="$2"

    ${r9cc} "$input" > tmp.s
    gcc -static -o tmp tmp.s
    ./tmp
    actual="$?"

    if [ "$actual" == "$expected" ]; then
        echo "$input => $actual"
    else
        echo "$input expected, but got $actual"
        exit 1
    fi
}

cargo build

try 0 0
try 42 42
try 21 '5+20-4'
try 41 ' 12 + 34 -5 '
try 36 '1+2+3+4+5+6+7+8'
try 153 '1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17'

echo OK
