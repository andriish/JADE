C   06/11/79 001251638  MEMBER NAME  ANO132   (S)           FORTRAN
      SUBROUTINE ANO132(IRUN)
C---  ANOMALIES OF RUNS 2134 - 2138, 2145
C---  15.12 GEV      OCTOBER 1979
C
C     H.WRIEDT    27.07.79     02:20
C     LAST MODIFICATION     25.01.80     16:35
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON /CPHASE/ LIMEVT,KCONST(192),LIMIT(192),LCHANN(192),
     +                LTHRES(192),LDEAD(192),LSUB(240)
      LOGICAL LCHANN,LTHRES,LDEAD,LSUB
C
C---  LCHANN:  CHANNELS WITH CONSTANT PEDESTALS
C---  LDEAD:   CHANNELS WHICH MUST BE SET TO 0 (E.G. FOR MISSING BITS
C              IN THE ADC
C---  KCONST:  CHANNELS FOR WHICH THE SUBTRACTION CONSTANT MUST BE
C              HIGHER (-1) OR LOWER (+1) THAN THE PROGRAM WOULD
C              AUTOMATICALLY CALCULATE
C---  LIMIT:   GIVES THE LOWER BOUND OF THE SUBTRACTION CONSTANT
C---  LTHRES:  CHANNELS FOR WHICH THE LOWER CUT BOUND IS SET TO 100
C              INSTEAD OF 50
C
      GOTO (2134,2135,2136,2137,2138,13,13,13,13,13,13,2145),IRUN
   13 RETURN
C
 2134 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(79) = +1
      LIMIT(79) = 390
      KCONST(107) = +1
      LIMIT(107) = 345
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(173) = +1
      LIMIT(173) = 450
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
C2135 PRELIMINARY
 2135 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(79) = +1
      LIMIT(79) = 390
      KCONST(101) = -1
      LIMIT(101) = 105
      KCONST(116) = -1
      LIMIT(116) = 135
      KCONST(162) = -1
      LIMIT(162) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(190) = -1
      LIMIT(190) = 150
C
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
C2136 PRELIMINARY
 2136 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(79) = +1
      LIMIT(79) = 390
      KCONST(107) = +1
      LIMIT(107) = 345
      KCONST(116) = -1
      LIMIT(116) = 135
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(173) = +1
      LIMIT(173) = 450
C
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2137 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2138 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2145 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
      END