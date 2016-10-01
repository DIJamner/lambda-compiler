mkdir -p lib_compiled
nasm -f macho64 lib/gc.s -o lib_compiled/gc.o
nasm -f macho64 lib/lib.s -o lib_compiled/lib.o