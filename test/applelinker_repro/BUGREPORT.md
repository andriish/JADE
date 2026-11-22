Subject: ld: common symbols larger than 64 KiB are linked with the wrong size (only the low 16 bits of the sizes seem to be compared)

Hello,

When the same common (tentative) symbol is defined in several object files with different sizes, the macOS linker keeps the wrong size. It looks like only the low 16 bits of the sizes are compared, so a 131072 byte symbol is linked with the size of a 400 byte declaration. No error or warning is printed, and the program writes past the end of the shrunken symbol into whatever follows.

ENVIRONMENT
  macOS 27.2.1 (26B131), arm64 (Apple M1)
  ld: PROJECT:ld-27106.1, BUILD 19:06:34 Oct 19 2026 (LTO LLVM 21.0.0, tapi-2100.3.8.1)
  SDK MacOSX 27.2, Command Line Tools 27.2.0.0.1792721180 (no Xcode)
  clang: Apple clang 21.0.0 (clang-2100.4.1.1), arm64-apple-darwin27.2.0
  -ld_classic is no longer accepted. Fortran: gfortran 16.2 (Homebrew), which always emits COMMON blocks as common symbols.

REPRODUCER (attached; run `make FC=gfortran-16`, the Fortran part is skipped without gfortran)
  a.c:    int xx[32768];   void fa(void) { xx[32767] = 1; }
  b.c:    int xx[100];     void fb(void) { xx[99] = 2; }
  main.c: extern void fa(void), fb(void);  int main(void) { fa(); fb(); return 0; }
    cc -fcommon -c main.c a.c b.c
    cc -o c_ab main.o a.o b.o
    nm -n -m c_ab; size -m c_ab
  Fortran: COMMON /XX/ A(32768) in one file, COMMON /XX/ B(100) in another.

EXPECTED  xx is 131072 bytes, the largest definition (GNU ld does this).
ACTUAL    xx is 400 bytes, for both link orders (a.o b.o, b.o a.o), in C and in Fortran:

  C:       0000000100004000 (__DATA,__common) external _xx
           Section __common: 400 (zerofill)
  Fortran: 0000000100008000 (__DATA,__common) external _xx_
           Section __common: 400 (zerofill)

  `otool -l` shows the same section size, 0x190.

In a real Fortran program these pairs were seen (the larger one loses):
  131072 (0x20000) vs   400 (0x190)
  167440 (0x28E10) vs 40000 (0x9C40)
Sizes below 64 KiB are merged correctly. A 20 MB COMMON block was linked as 40 KB, causing segmentation faults and bus errors in library code, and another one shrunk to 40000 bytes corrupted the data after it. Nothing is reported at link time, so the failure shows up far from the cause.

Please keep the largest size when merging common symbols, or diagnose a size mismatch with a warning or error. Please also send me the Feedback/radar number.

Thank you,
[name]
