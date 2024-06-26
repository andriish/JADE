C   09/10/79 002161540  MEMBER NAME  ANOM69   (S)           FORTRAN
      SUBROUTINE ANOM69(IRUN)
C---  ANOMALIES OF RUNS 1711 - 1713
C---  15.33 GEV      OCTOBER 1979
C
C     H.WRIEDT    27.07.79     02:20
C     LAST MODIFICATION     16.02.80     15:40
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
      GOTO (1711,1712,1713),IRUN
   13 RETURN
C
 1711 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 105
      KCONST(162) = -1
      LIMIT(162) = 75
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(183) = -1
      LIMIT(183) = 135
      KCONST(185) = -1
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 300
      KCONST(190) = -1
      LIMIT(190) = 135
C
      LTHRES(25) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(125) = .TRUE.
      LTHRES(147) = .TRUE.
      LTHRES(148) = .TRUE.
      LTHRES(151) = .TRUE.
      LTHRES(155) = .TRUE.
      LTHRES(158) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(164) = .TRUE.
      LTHRES(165) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(169) = .TRUE.
      LTHRES(175) = .TRUE.
      LTHRES(178) = .TRUE.
      LTHRES(179) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1712 PRELIMINARY
 1712 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 105
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(190) = -1
      LIMIT(190) = 150
C
      LTHRES(25) = .TRUE.
      LTHRES(39) = .TRUE.
      LTHRES(107) = .TRUE.
      LTHRES(147) = .TRUE.
      LTHRES(165) = .TRUE.
      LTHRES(169) = .TRUE.
      LTHRES(173) = .TRUE.
      LTHRES(177) = .TRUE.
C
      RETURN
C
C1713 PRELIMINARY
 1713 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 105
      KCONST(162) = -1
      LIMIT(162) = 90
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(183) = -1
      LIMIT(183) = 135
      KCONST(190) = -1
      LIMIT(190) = 150
C
      LTHRES(25) = .TRUE.
      LTHRES(39) = .TRUE.
      LTHRES(107) = .TRUE.
      LTHRES(147) = .TRUE.
      LTHRES(165) = .TRUE.
      LTHRES(173) = .TRUE.
      LTHRES(177) = .TRUE.
C
      RETURN
C
      END
