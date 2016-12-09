#! /bin/bash
# TODO: the following line is copied from StackOverflow. What does it do? (it's probably wrong)
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR
#TODO: grab filename only
mkdir -p "$2"
mkdir "$2/tmp"
#nasm -f macho64 gc.s -o "$2/gc.o"
pushd gc
cargo rustc -- --crate-type=staticlib
cp target/debug/deps/libgc.a "../$2/tmp/libgc.a"
popd
nasm -f macho64 lib.s -o "$2/tmp/lib.o"
nasm -f macho64 "$1" -o "$1.o"
pushd "$2/tmp"
#TODO: switch off of debug
ld -L . -no_pie -demangle -dynamic -arch x86_64 -macosx_version_min 10.11.0 -o ../main lib.o "$1.o" -lc -lgc -lSystem -lm
cd ..
./main
echo ""
popd
rm -r "$2/tmp"
