C   09/10/79 912071503  MEMBER NAME  ANOM66   (S)           FORTRAN
      SUBROUTINE ANOM66(IRUN)
C---  ANOMALIES OF RUNS 1639 - 1648
C---  15.30 GEV      SEPTEMBER 1979
C
C     H.WRIEDT    27.07.79     02:20
C     LAST MODIFICATION     07.12.79     15:00
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
      GOTO (1639,1640,1641,1642,1643,1644,1645,1646,1647,1648),IRUN
   13 RETURN
C
 1639 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(185) = -1
      LIMIT(185) = 300
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(80) = .TRUE.
C
      RETURN
C
 1640 LCHANN(79) = .TRUE.
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
      KCONST(178) = -1
      LIMIT(178) = 150
      KCONST(183) = -1
      LIMIT(183) = 135
      KCONST(184) = -1
      LIMIT(184) = 120
      KCONST(190) = -1
      LIMIT(190) = 150
C
      LTHRES(50) = .TRUE.
      LTHRES(52) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(54) = .TRUE.
      LTHRES(56) = .TRUE.
      LTHRES(57) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(61) = .TRUE.
      LTHRES(62) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(64) = .TRUE.
      LTHRES(65) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(70) = .TRUE.
      LTHRES(71) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(73) = .TRUE.
      LTHRES(74) = .TRUE.
      LTHRES(75) = .TRUE.
      LTHRES(76) = .TRUE.
      LTHRES(77) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(81) = .TRUE.
      LTHRES(82) = .TRUE.
      LTHRES(83) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(86) = .TRUE.
      LTHRES(87) = .TRUE.
      LTHRES(88) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(90) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(92) = .TRUE.
      LTHRES(93) = .TRUE.
      LTHRES(94) = .TRUE.
C
      RETURN
C
 1641 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 105
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 1642 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 105
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 1643 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 105
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
      KCONST(190) = -1
      LIMIT(190) = 120
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(50) = .TRUE.
      LTHRES(52) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(54) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(56) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(62) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(64) = .TRUE.
      LTHRES(65) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(70) = .TRUE.
      LTHRES(71) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(76) = .TRUE.
      LTHRES(77) = .TRUE.
      LTHRES(80) = .TRUE.
      LTHRES(81) = .TRUE.
      LTHRES(82) = .TRUE.
      LTHRES(83) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(86) = .TRUE.
      LTHRES(87) = .TRUE.
      LTHRES(88) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(90) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(92) = .TRUE.
      LTHRES(93) = .TRUE.
      LTHRES(94) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
C
      RETURN
C
 1644 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 105
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 135
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
      KCONST(190) = -1
      LIMIT(190) = 135
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(50) = .TRUE.
      LTHRES(52) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(54) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(56) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(61) = .TRUE.
      LTHRES(62) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(64) = .TRUE.
      LTHRES(65) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(70) = .TRUE.
      LTHRES(71) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(76) = .TRUE.
      LTHRES(77) = .TRUE.
      LTHRES(80) = .TRUE.
      LTHRES(81) = .TRUE.
      LTHRES(82) = .TRUE.
      LTHRES(83) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(86) = .TRUE.
      LTHRES(87) = .TRUE.
      LTHRES(88) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(92) = .TRUE.
      LTHRES(93) = .TRUE.
      LTHRES(94) = .TRUE.
C
      RETURN
C
 1645 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 105
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 1646 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 105
      KCONST(162) = -1
      LIMIT(162) = 105
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(178) = -1
      LIMIT(178) = 150
      KCONST(183) = -1
      LIMIT(183) = 120
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 1647 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(185) = -1
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 300
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 1648 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 105
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(185) = -1
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 300
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
      END
