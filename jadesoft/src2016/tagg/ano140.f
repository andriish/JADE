C   06/11/79 001231603  MEMBER NAME  ANO140   (S)           FORTRAN
      SUBROUTINE ANO140(IRUN)
C---  ANOMALIES OF RUNS 2177 - 2182
C---  15.20 GEV      OCTOBER 1979
C
C     H.WRIEDT    27.07.79     02:20
C     LAST MODIFICATION     23.01.80     16:05
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON /CPHASE/ LIMEVT,KCONST(192),LIMIT(192),LCHANN(192),
     &                LTHRES(192),LDEAD(192),LSUB(240)
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
      GOTO (2177,2178,2179,2180,2181,2182),IRUN
   13 RETURN
C
 2177 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(162) = -1
      LIMIT(162) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(190) = -1
      LIMIT(190) = 150
C
      LTHRES(183) = .TRUE.
C
      RETURN
C
 2178 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(162) = -1
      LIMIT(162) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(183) = -1
      LIMIT(183) = 135
      KCONST(190) = -1
      LIMIT(190) = 150
C
      LTHRES(176) = .TRUE.
      LTHRES(178) = .TRUE.
C
      RETURN
C
C2179 PRELIMINARY
 2179 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(162) = -1
      LIMIT(162) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(183) = -1
      LIMIT(183) = 135
      KCONST(190) = -1
      LIMIT(190) = 150
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(176) = .TRUE.
      LTHRES(178) = .TRUE.
C
      RETURN
C
C2180 PRELIMINARY
 2180 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(176) = .TRUE.
      LTHRES(178) = .TRUE.
C
      RETURN
C
C2181 PRELIMINARY
 2181 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(176) = .TRUE.
      LTHRES(178) = .TRUE.
C
      RETURN
C
C2182 PRELIMINARY
 2182 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(176) = .TRUE.
      LTHRES(178) = .TRUE.
C
      RETURN
C
      END
