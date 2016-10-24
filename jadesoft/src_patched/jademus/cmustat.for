C----------- START OF MACRO CMUSTAT ------------------------------------
C   LAST CHANGE 12.47 15/06/79 JOHN ALLISON.
C      /CMUSTT/
C
      COMMON / CMUSTT / MUTIT(100),NMU(100)
      character*8 MUTIT, MUT1(50), MUT2(50)
      EQUIVALENCE(MUTIT(1),MUT1(1)),(MUTIT(51),MUT2(1))
C
C  NMU ARE USED FOR STATISTICS COUNTING IN THE MUON ROUTINES
C
C------------ END OF MACRO CMUSTAT -------------------------------------
