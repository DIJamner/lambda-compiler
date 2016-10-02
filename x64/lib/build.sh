rm -r ../lib_compiled
mkdir -p ../lib_compiled
nasm -f macho64 gc.s -l ../lib_compiled/gc.lst -o ../lib_compiled/gc.o
nasm -f macho64 lib.s -l ../lib_compiled/lib.lst -o ../lib_compiled/lib.o
nasm -f macho64 "$1.s" -l "../lib_compiled/$1.lst" -o "../lib_compiled/$1.o"
pushd ../lib_compiled
#gcc gc.o lib.o test.o -v
ld -no_pie -demangle -dynamic -arch x86_64 -macosx_version_min 10.11.0 -o test gc.o lib.o test.o -lc
./"$1"
echo ""
