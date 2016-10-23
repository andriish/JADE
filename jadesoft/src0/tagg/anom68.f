C   09/10/79 002161537  MEMBER NAME  ANOM68   (S)           FORTRAN
      SUBROUTINE ANOM68(IRUN)
C---  ANOMALIES OF RUNS 1662 - 1686
C---  15.32 GEV      SEPTEMBER 1979
C
C     H.WRIEDT    27.07.79     02:20
C     LAST MODIFICATION     16.02.80    15:35
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
      GOTO (1662,13,1664,1665,1666,1667,13,1669,13,1671,13,1673,1674,13,
     &      1676,1677,1678,1679,1680,1681,1682,1683,1684,1685,1686),IRUN
   13 RETURN
C
 1662 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(122) = .TRUE.
C
      RETURN
C
C1664 PRELIMINARY
 1664 LCHANN(79) = .TRUE.
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
      KCONST(185) = -1
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 300
      KCONST(190) = -1
      LIMIT(190) = 135
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(122) = .TRUE.
C
      RETURN
C
C1665 PRELIMINARY
 1665 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(122) = .TRUE.
C
      RETURN
C
C1666 PRELIMINARY
 1666 LCHANN(79) = .TRUE.
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
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(122) = .TRUE.
C
      RETURN
C
C1667 PRELIMINARY
 1667 LCHANN(79) = .TRUE.
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
      KCONST(190) = -1
      LIMIT(190) = 135
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(122) = .TRUE.
C
      RETURN
C
 1669 LCHANN(79) = .TRUE.
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
      KCONST(178) = -1
      LIMIT(178) = 120
      KCONST(183) = -1
      LIMIT(183) = 120
      KCONST(184) = -1
      LIMIT(184) = 105
      KCONST(190) = -1
      LIMIT(190) = 135
C
      LTHRES(50) = .TRUE.
      LTHRES(52) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(54) = .TRUE.
      LTHRES(56) = .TRUE.
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
      LTHRES(74) = .TRUE.
      LTHRES(76) = .TRUE.
      LTHRES(77) = .TRUE.
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
 1671 LCHANN(79) = .TRUE.
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
      LIMIT(187) = 300
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(50) = .TRUE.
      LTHRES(52) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(54) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(61) = .TRUE.
      LTHRES(62) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(64) = .TRUE.
      LTHRES(65) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(68) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(70) = .TRUE.
      LTHRES(71) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(73) = .TRUE.
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
 1673 LCHANN(79) = .TRUE.
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
      LTHRES(59) = .TRUE.
      LTHRES(64) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(73) = .TRUE.
      LTHRES(77) = .TRUE.
      LTHRES(83) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(93) = .TRUE.
      LTHRES(94) = .TRUE.
C
      RETURN
C
C1674 PRELIMINARY
 1674 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(59) = .TRUE.
      LTHRES(64) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(73) = .TRUE.
      LTHRES(77) = .TRUE.
      LTHRES(83) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(93) = .TRUE.
      LTHRES(94) = .TRUE.
C
      RETURN
C
C1676 PRELIMINARY
 1676 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(64) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(73) = .TRUE.
      LTHRES(77) = .TRUE.
      LTHRES(80) = .TRUE.
      LTHRES(83) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(93) = .TRUE.
      LTHRES(94) = .TRUE.
      LTHRES(122) = .TRUE.
C
      RETURN
C
 1677 LCHANN(79) = .TRUE.
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
      LIMIT(190) = 120
C
      LTHRES(67) = .TRUE.
      LTHRES(165) = .TRUE.
      LTHRES(166) = .TRUE.
      LTHRES(167) = .TRUE.
C
      RETURN
C
 1678 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 105
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(187) = -1
      LIMIT(187) = 300
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(64) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(73) = .TRUE.
      LTHRES(77) = .TRUE.
      LTHRES(83) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(93) = .TRUE.
      LTHRES(94) = .TRUE.
      LTHRES(112) = .TRUE.
C
      RETURN
C
C1679 PRELIMINARY
 1679 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 105
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(59) = .TRUE.
      LTHRES(64) = .TRUE.
      LTHRES(73) = .TRUE.
      LTHRES(77) = .TRUE.
      LTHRES(83) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(93) = .TRUE.
      LTHRES(94) = .TRUE.
C
      RETURN
C
 1680 LCHANN(79) = .TRUE.
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
      LTHRES(53) = .TRUE.
      LTHRES(54) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(64) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(82) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(88) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(92) = .TRUE.
      LTHRES(93) = .TRUE.
      LTHRES(94) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
C1681 PRELIMINARY
 1681 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(162) = -1
      LIMIT(162) = 45
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(183) = -1
      LIMIT(183) = 120
      KCONST(190) = -1
      LIMIT(190) = 120
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(54) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(64) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(82) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(88) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(92) = .TRUE.
      LTHRES(93) = .TRUE.
      LTHRES(94) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
 1682 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(185) = -1
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 300
C
      RETURN
C
 1683 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(190) = -1
      LIMIT(190) = 135
C
      LTHRES(1) = .TRUE.
      LTHRES(7) = .TRUE.
      LTHRES(18) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(32) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(46) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(80) = .TRUE.
      LTHRES(122) = .TRUE.
C
      RETURN
C
C1684 PRELIMINARY
 1684 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(162) = -1
      LIMIT(162) = 75
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(185) = -1
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 300
      KCONST(190) = -1
      LIMIT(190) = 135
C
      RETURN
C
C1685 PRELIMINARY
 1685 LCHANN(79) = .TRUE.
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
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 1686 LCHANN(79) = .TRUE.
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
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
      END
