Subject: ld (ld-prime): tentative definitions / common symbols larger than 64 KiB are linked with the wrong size (only the low 16 bits of the sizes are compared)

Hello,

the macOS linker truncates common symbols when the same symbol is defined as a common (tentative) definition in several object files with different sizes. The size of the result is decided by comparing only the low 16 bits of the sizes, so a symbol of, for example, 131072 bytes can be linked with the size of a 400 byte declaration. The program then has a symbol that is smaller than the code compiled against it expects, and writes past its end into whatever follows. No error or warning is printed.

ENVIRONMENT
  macOS 27.2.1 (build 26B131), arm64 (Apple M1)
  ld: @(#)PROGRAM:ld PROJECT:ld-27106.1, BUILD 19:06:34 Oct 19 2026
      (LTO support using LLVM version 21.0.0; TAPI version 21.0.0 (tapi-2100.3.8.1))
  SDK: MacOSX 27.2 (/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk)
  Command Line Tools: 27.2.0.0.1792721180 (no Xcode installed)
  clang: Apple clang version 21.0.0 (clang-2100.4.1.1), target arm64-apple-darwin27.2.0
  -ld_classic is no longer accepted, so there is no way to select another linker.
  Fortran: GCC/gfortran 16.2 (Homebrew), which always emits COMMON blocks as common symbols.
  C: clang with -fcommon (clang defaults to -fno-common).

SUMMARY OF THE BEHAVIOUR
  Two objects define the same common symbol:
      a.o: xx, 131072 bytes (0x20000)
      b.o: xx,    400 bytes (0x190)
  Linked size of xx: 400 bytes. Expected: 131072 bytes (the largest, which is what GNU ld does).
  Observed sizes from the Fortran form of this case (the larger one loses):
      131072 (low 16 bits 0x0000) vs   400 (0x0190): the 131072 byte one loses
      167440 (0x28E10, low 16 bits 0x8E10) vs 40000 (0x9C40): the 167440 byte one loses
  This is consistent with the comparison being done on (size & 0xFFFF). Common symbols whose sizes are below 64 KiB are merged correctly.

STEPS TO REPRODUCE
  The attached directory has a Makefile; `make` builds and prints the linked size of xx for three link variants, in C and in Fortran (the Fortran part is skipped if no gfortran is installed).

  a.c:   int xx[32768];            void fa(void) { xx[32767] = 1; }
  b.c:   int xx[100];              void fb(void) { xx[99] = 2; }
  main.c: extern void fa(void), fb(void);  int main(void) { fa(); fb(); return 0; }

      cc -fcommon -c main.c a.c b.c
      cc -o c_ab main.o a.o b.o
      nm -n -m c_ab | grep ' _xx$'      # shows (common)/section of xx
      size -m c_ab                       # size of the __common / __bss section

  Fortran equivalent: `COMMON /XX/ A(32768)` in one file and `COMMON /XX/ B(100)` in another.

EXPECTED RESULT
  xx has 131072 bytes (the size of the largest definition).

ACTUAL RESULT
  xx has 400 bytes. Output of `make FC=gfortran-16` on the machine above:

  == C, link order a.o b.o     (expected 131072)
  0000000100004000 (__DATA,__common) external _xx
          Section __common: 400 (zerofill)
  == C, link order b.o a.o     (expected 131072)
  0000000100004000 (__DATA,__common) external _xx
          Section __common: 400 (zerofill)
  == C, workaround wa.o b.o    (expected 131072)
  0000000100004000 (__DATA,__common) external _xx
          Section __common: 131072 (zerofill)
  == Fortran, link order a.o b.o  (expected 131072)
  0000000100008000 (__DATA,__common) external _xx_
          Section __common: 400 (zerofill)
  == Fortran, link order b.o a.o  (expected 131072)
  0000000100008000 (__DATA,__common) external _xx_
          Section __common: 400 (zerofill)
  == Fortran, BLOCK DATA workaround (expected 131072)
  0000000100008010 (__DATA,__common) external _xx_
          Section __common: 131088 (zerofill)

  (`otool -l c_ab` shows the same: section __common, size 0x190.)
  The result does not depend on the link order. In the Fortran workaround case the section is
  16 bytes larger because another 16-byte symbol precedes xx_ (xx_ starts at section offset
  0x10); xx_ itself has 131072 bytes.
  In a real program (several MB of statically allocated Fortran COMMON blocks) this showed up as segmentation faults and bus errors inside library code that uses a 20 MB COMMON block which the linker had shrunk to 40 KB, and as silent corruption of the data following the symbol (a second COMMON block had been shrunk from 167440 to 40000 bytes). Because nothing is reported at link time, the failure appears far away from the cause.

WORKAROUND
  Give the symbol a real (non-tentative) definition of the full size in an object that is linked directly into the executable: a C definition with an initialiser (`int xx[32768] = {0};`, goes to zero-fill, no file size) or a Fortran BLOCK DATA unit that initialises one element (`DATA A(1) /0./`). A real definition always wins over common definitions, whatever the sizes. The BLOCK DATA unit must be in an object that is linked directly (a unit in a static library is not pulled in). `make` shows this as the third variant.

REQUEST
  Please fix the merge of common symbols so that the largest size is kept (or, if truncation is intended for some reason, diagnose it with a warning/error), and let me know the Feedback Assistant / radar number.

Thank you,
[name]
