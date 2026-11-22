# Reproducer for the Apple linker bug: tentative definitions (C common symbols, Fortran COMMON
# blocks) of the same name are merged comparing only the low 16 bits of their sizes.
# Expected linked size of xx: 131072 bytes (the largest). Buggy linker: 400 bytes.
# Usage: make          build and report (C, and Fortran if gfortran is found)
#        make clean
CC      ?= cc
# make's built-in default for FC is f77
ifeq ($(origin FC),default)
FC      = gfortran
endif
CFLAGS  += -fcommon -O0
FFLAGS  += -O0

all: c_ab c_ba c_wa
	@echo "== C, link order a.o b.o     (expected 131072)"; ./linked_size.sh c_ab
	@echo "== C, link order b.o a.o     (expected 131072)"; ./linked_size.sh c_ba
	@echo "== C, workaround wa.o b.o    (expected 131072)"; ./linked_size.sh c_wa
	@if command -v $(FC) >/dev/null 2>&1; then $(MAKE) fortran; else echo "(no $(FC): Fortran part skipped)"; fi

fortran: f_ab f_ba f_wa
	@echo "== Fortran, link order a.o b.o  (expected 131072)"; ./linked_size.sh f_ab
	@echo "== Fortran, link order b.o a.o  (expected 131072)"; ./linked_size.sh f_ba
	@echo "== Fortran, BLOCK DATA workaround (expected 131072)"; ./linked_size.sh f_wa

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@
%.fo: %.f
	$(FC) $(FFLAGS) -c $< -o $@

c_ab: main.o a.o b.o
	$(CC) -o $@ main.o a.o b.o
c_ba: main.o b.o a.o
	$(CC) -o $@ main.o b.o a.o
c_wa: main.o wa.o b.o a.o
	$(CC) -o $@ main.o wa.o b.o a.o
f_ab: main.fo a.fo b.fo
	$(FC) -o $@ main.fo a.fo b.fo
f_ba: main.fo b.fo a.fo
	$(FC) -o $@ main.fo b.fo a.fo
f_wa: main.fo wa.fo b.fo a.fo
	$(FC) -o $@ main.fo wa.fo b.fo a.fo

clean:
	rm -f *.o *.fo c_ab c_ba c_wa f_ab f_ba f_wa
