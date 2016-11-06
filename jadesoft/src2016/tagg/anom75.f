C   09/10/79 912201715  MEMBER NAME  ANOM75   (S)           FORTRAN
      SUBROUTINE ANOM75(IRUN)
C---  ANOMALIES OF RUNS 1756 - 1762
C---  15.39 GEV      OCTOBER 1979
C
C     H.WRIEDT    27.07.79     02:20
C     LAST MODIFICATION     19.12.79     14:30
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
      GOTO (1756,1757,1758,1759,1760,1761,1762),IRUN
   13 RETURN
C
 1756 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(2) = -1
      LIMIT(2) = 255
      KCONST(4) = -1
      LIMIT(4) = 270
      KCONST(7) = -1
      LIMIT(7) = 120
      KCONST(8) = -1
      LIMIT(8) = 225
      KCONST(11) = -1
      LIMIT(11) = 195
      KCONST(23) = -1
      LIMIT(23) = 150
      KCONST(25) = -1
      LIMIT(25) = 255
      KCONST(26) = -1
      LIMIT(26) = 240
      KCONST(27) = -1
      LIMIT(27) = 210
      KCONST(29) = -1
      LIMIT(29) = 210
      KCONST(33) = -1
      LIMIT(33) = 195
      KCONST(35) = -1
      LIMIT(35) = 255
      KCONST(42) = -1
      LIMIT(42) = 225
      KCONST(43) = -1
      LIMIT(43) = 225
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(15) = .TRUE.
      LTHRES(16) = .TRUE.
      LTHRES(17) = .TRUE.
      LTHRES(21) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(27) = .TRUE.
      LTHRES(38) = .TRUE.
      LTHRES(41) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(44) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(80) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(122) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(158) = .TRUE.
      LTHRES(160) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(178) = .TRUE.
C
      RETURN
C
 1757 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      RETURN
C
 1758 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(79) = +1
      LIMIT(79) = 390
      KCONST(107) = +1
      LIMIT(107) = 360
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(173) = +1
      LIMIT(173) = 450
      KCONST(183) = -1
      LIMIT(183) = 120
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
      KCONST(190) = -1
      LIMIT(190) = 120
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(183) = .TRUE.
C
      RETURN
C
 1759 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(79) = +1
      LIMIT(79) = 390
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(173) = +1
      LIMIT(173) = 450
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      RETURN
C
 1760 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
      KCONST(190) = -1
      LIMIT(190) = 135
C
      RETURN
C
 1761 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(178) = -1
      LIMIT(178) = 120
      KCONST(184) = -1
      LIMIT(184) = 120
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
      KCONST(190) = -1
      LIMIT(190) = 150
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(51) = .TRUE.
      LTHRES(52) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(80) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(154) = .TRUE.
C
      RETURN
C
C1762 PRELIMINARY
 1762 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(185) = -1
      LIMIT(185) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(51) = .TRUE.
      LTHRES(52) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(80) = .TRUE.
      LTHRES(112) = .TRUE.
C
      RETURN
C
      END
