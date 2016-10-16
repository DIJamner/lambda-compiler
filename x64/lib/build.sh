#! /bin/bash
alias nasm2=/usr/local/bin/nasm
#TODO: grab filename only
mkdir -p "$2"
#nasm -f macho64 gc.s -o "$2/gc.o"
pushd gc
cargo rustc -- --crate-type=staticlib
cp target/debug/deps/libgc.a "../$2/libgc.a"
popd
nasm -f macho64 lib.s -o "$2/lib.o"
nasm -f macho64 "$1" -o "$2/$1.o"
pushd "$2"
#TODO: switch off of debug
ld -L . -no_pie -demangle -dynamic -arch x86_64 -macosx_version_min 10.11.0 -o main lib.o "$1.o" -lc -lgc -lSystem -lm
./main
echo ""
popd
#rm -r "$2"
