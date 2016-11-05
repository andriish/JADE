C   09/10/79 002051543  MEMBER NAME  ANOM64   (S)           FORTRAN
      SUBROUTINE ANOM64(IRUN)
C---  ANOMALIES OF RUNS 1609 - 1626
C---  15.28 GEV      SEPTEMBER 1979
C
C     H.WRIEDT    27.07.79     02:20
C     LAST MODIFICATION     05.02.80     15:40
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON /CPHASE/ LIMEVT,KCONST(192),LIMIT(192),LCHANN(192),
     *                LTHRES(192),LDEAD(192),LSUB(240)
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
      GOTO (1609,1610,1611,1612,1613,13,13,1616,1617,1618,1619,1620,
     *      1621,1622,1623,13,1625,1626),IRUN
   13 RETURN
C
 1609 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(49)  = -1
      LIMIT(49) = 75
      KCONST(97)  = -1
      LIMIT(97) = 240
      KCONST(98)  = -1
      LIMIT(98) = 390
      KCONST(99)  = -1
      LIMIT(99) = 330
      KCONST(100)  = -1
      LIMIT(100) = 390
      KCONST(101)  = -1
      LIMIT(101) = 120
      KCONST(103)  = -1
      LIMIT(103) = 300
      KCONST(123)  = -1
      LIMIT(123) = 420
      KCONST(125)  = -1
      LIMIT(125) = 360
      KCONST(130)  = -1
      LIMIT(130) = 330
      KCONST(133)  = -1
      LIMIT(133) = 390
      KCONST(134)  = -1
      LIMIT(134) = 480
      KCONST(140)  = -1
      LIMIT(140) = 510
      KCONST(166)  = -1
      LIMIT(166) = 420
      KCONST(185)  = -1
      LIMIT(185) = 360
C
      RETURN
C
 1610 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(154)  = -1
      LIMIT(154) = 60
      KCONST(166)  = -1
      LIMIT(166) = 420
C
      RETURN
C
 1611 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(49)  = -1
      LIMIT(49) = 60
      KCONST(51)  = -1
      LIMIT(51) = 90
      KCONST(55)  = -1
      LIMIT(55) = 45
      KCONST(67)  = -1
      LIMIT(67) = 30
      KCONST(68)  = -1
      LIMIT(68) = 60
      KCONST(166)  = -1
      LIMIT(166) = 435
C
      RETURN
C
 1612 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(49)  = -1
      LIMIT(49) = 75
      KCONST(51)  = -1
      LIMIT(51) = 90
      KCONST(55)  = -1
      LIMIT(55) = 45
      KCONST(68)  = -1
      LIMIT(68) = 75
      KCONST(166)  = -1
      LIMIT(166) = 450
C
      RETURN
C
 1613 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(97)  = -1
      LIMIT(97) = 240
      KCONST(117)  = -1
      LIMIT(117) = 60
      KCONST(148)  = -1
      LIMIT(148) = 120
      KCONST(150)  = -1
      LIMIT(150) = 330
      KCONST(153)  = -1
      LIMIT(153) = 300
      KCONST(155)  = -1
      LIMIT(155) = 270
      KCONST(166)  = -1
      LIMIT(166) = 450
      KCONST(171)  = -1
      LIMIT(171) = 150
      KCONST(177)  = -1
      LIMIT(177) = 105
C
      RETURN
C
 1616 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(49)  = -1
      LIMIT(49) = 90
      KCONST(51)  = -1
      LIMIT(51) = 105
      KCONST(55)  = -1
      LIMIT(55) = 60
      KCONST(67)  = -1
      LIMIT(67) = 45
      KCONST(68)  = -1
      LIMIT(68) = 75
      KCONST(166)  = -1
      LIMIT(166) = 420
      KCONST(185)  = -1
      LIMIT(185) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(41) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(51) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(68) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(101) = .TRUE.
      LTHRES(129) = .TRUE.
      LTHRES(176) = .TRUE.
C
      RETURN
C
 1617 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(49)  = -1
      LIMIT(49) = 120
      KCONST(67)  = -1
      LIMIT(67) = 30
      KCONST(68)  = -1
      LIMIT(68) = 75
      KCONST(75)  = -1
      LIMIT(75) = 30
      KCONST(97)  = -1
      LIMIT(97) = 270
      KCONST(98)  = -1
      LIMIT(98) = 390
      KCONST(99)  = -1
      LIMIT(99) = 300
      KCONST(100)  = -1
      LIMIT(100) = 360
      KCONST(103)  = -1
      LIMIT(103) = 270
      KCONST(123)  = -1
      LIMIT(123) = 390
      KCONST(125)  = -1
      LIMIT(125) = 330
      KCONST(130)  = -1
      LIMIT(130) = 300
      KCONST(133)  = -1
      LIMIT(133) = 330
      KCONST(134)  = -1
      LIMIT(134) = 420
      KCONST(140)  = -1
      LIMIT(140) = 480
      KCONST(166)  = -1
      LIMIT(166) = 420
      KCONST(185)  = -1
      LIMIT(185) = 390
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(51) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(61) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(68) = .TRUE.
C
      RETURN
C
C1618 PRELIMINARY
 1618 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(97)  = -1
      LIMIT(97) = 255
      KCONST(166)  = -1
      LIMIT(166) = 420
      KCONST(185)  = -1
      LIMIT(185) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(51) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(61) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(68) = .TRUE.
C
      RETURN
C
C1619 PRELIMINARY
 1619 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(97)  = -1
      LIMIT(97) = 255
      KCONST(117)  = -1
      LIMIT(117) = 45
      KCONST(166)  = -1
      LIMIT(166) = 420
      KCONST(185)  = -1
      LIMIT(185) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(51) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(61) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(68) = .TRUE.
C
      RETURN
C
C1620 PRELIMINARY
 1620 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(49)  = -1
      LIMIT(49) = 45
      KCONST(51)  = -1
      LIMIT(51) = 75
      KCONST(55)  = -1
      LIMIT(55) = 30
      KCONST(61)  = -1
      LIMIT(61) = 105
      KCONST(67)  = -1
      LIMIT(67) = 15
      KCONST(68)  = -1
      LIMIT(68) = 60
      KCONST(97)  = -1
      LIMIT(97) = 255
      KCONST(101)  = -1
      LIMIT(101) = 195
      KCONST(106)  = -1
      LIMIT(106) = 225
      KCONST(109)  = -1
      LIMIT(109) = 135
      KCONST(113)  = -1
      LIMIT(113) = 150
      KCONST(115)  = -1
      LIMIT(115) = 150
      KCONST(119)  = -1
      LIMIT(119) = 150
      KCONST(120)  = -1
      LIMIT(120) = 120
      KCONST(122)  = -1
      LIMIT(122) = 75
      KCONST(132)  = -1
      LIMIT(132) = 75
      KCONST(139)  = -1
      LIMIT(139) = 45
      KCONST(166)  = -1
      LIMIT(166) = 435
      KCONST(185)  = -1
      LIMIT(185) = 375
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(51) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(61) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(68) = .TRUE.
C
      RETURN
C
C1621 PRELIMINARY
 1621 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(97)  = -1
      LIMIT(97) = 255
      KCONST(98)  = -1
      LIMIT(98) = 315
      KCONST(99)  = -1
      LIMIT(99) = 285
      KCONST(100)  = -1
      LIMIT(100) = 315
      KCONST(101)  = -1
      LIMIT(101) = 165
      KCONST(102)  = -1
      LIMIT(102) = 195
      KCONST(103)  = -1
      LIMIT(103) = 255
      KCONST(105)  = -1
      LIMIT(105) = 255
      KCONST(106)  = -1
      LIMIT(106) = 210
      KCONST(108)  = -1
      LIMIT(108) = 105
      KCONST(109)  = -1
      LIMIT(109) = 135
      KCONST(110)  = -1
      LIMIT(110) = 255
      KCONST(111)  = -1
      LIMIT(111) = 210
      KCONST(113)  = -1
      LIMIT(113) = 150
      KCONST(115)  = -1
      LIMIT(115) = 135
      KCONST(118)  = -1
      LIMIT(118) = 165
      KCONST(119)  = -1
      LIMIT(119) = 150
      KCONST(120)  = -1
      LIMIT(120) = 120
      KCONST(122)  = -1
      LIMIT(122) = 45
      KCONST(123)  = -1
      LIMIT(123) = 360
      KCONST(124)  = -1
      LIMIT(124) = 165
      KCONST(125)  = -1
      LIMIT(125) = 285
      KCONST(128)  = -1
      LIMIT(128) = 30
      KCONST(129)  = -1
      LIMIT(129) = 45
      KCONST(130)  = -1
      LIMIT(130) = 270
      KCONST(131)  = -1
      LIMIT(131) = 165
      KCONST(132)  = -1
      LIMIT(132) = 75
      KCONST(133)  = -1
      LIMIT(133) = 315
      KCONST(134)  = -1
      LIMIT(134) = 420
      KCONST(136)  = -1
      LIMIT(136) = 180
      KCONST(137)  = -1
      LIMIT(137) = 195
      KCONST(138)  = -1
      LIMIT(138) = 150
      KCONST(139)  = -1
      LIMIT(139) = 30
      KCONST(140)  = -1
      LIMIT(140) = 450
      KCONST(141)  = -1
      LIMIT(141) = 105
      KCONST(142)  = -1
      LIMIT(142) = 225
      KCONST(150)  = -1
      LIMIT(150) = 345
      KCONST(166)  = -1
      LIMIT(166) = 450
      KCONST(185)  = -1
      LIMIT(185) = 375
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(51) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(61) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(68) = .TRUE.
C
      RETURN
C
C1622 PRELIMINARY
 1622 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(7)  = -1
      LIMIT(7) = 45
      KCONST(13)  = -1
      LIMIT(13) = 180
      KCONST(40)  = -1
      LIMIT(40) = 210
      KCONST(51)  = -1
      LIMIT(51) = 75
      KCONST(55)  = -1
      LIMIT(55) = 30
      KCONST(61)  = -1
      LIMIT(61) = 90
      KCONST(68)  = -1
      LIMIT(68) = 60
      KCONST(74)  = -1
      LIMIT(74) = 120
      KCONST(97)  = -1
      LIMIT(97) = 255
      KCONST(101)  = -1
      LIMIT(101) = 195
      KCONST(104)  = -1
      LIMIT(104) = 90
      KCONST(109)  = -1
      LIMIT(109) = 120
      KCONST(118)  = -1
      LIMIT(118) = 180
      KCONST(122)  = -1
      LIMIT(122) = 60
      KCONST(128)  = -1
      LIMIT(128) = 30
      KCONST(145)  = -1
      LIMIT(145) = 90
      KCONST(146)  = -1
      LIMIT(146) = 330
      KCONST(149)  = -1
      LIMIT(149) = 180
      KCONST(150)  = -1
      LIMIT(150) = 345
      KCONST(158)  = -1
      LIMIT(158) = 330
      KCONST(161)  = -1
      LIMIT(161) = 315
      KCONST(164)  = -1
      LIMIT(164) = 330
      KCONST(166)  = -1
      LIMIT(166) = 435
      KCONST(167)  = -1
      LIMIT(167) = 330
      KCONST(185)  = -1
      LIMIT(185) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(51) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(61) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(68) = .TRUE.
C
      RETURN
C
C1623 PRELIMINARY
 1623 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(97)  = -1
      LIMIT(97) = 255
      KCONST(117)  = -1
      LIMIT(117) = 75
      KCONST(166)  = -1
      LIMIT(166) = 435
      KCONST(185)  = -1
      LIMIT(185) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(51) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(61) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(68) = .TRUE.
C
      RETURN
C
C1625 PRELIMINARY
 1625 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116)  = -1
      LIMIT(116) = 120
      KCONST(166)  = -1
      LIMIT(166) = 360
      KCONST(181)  = -1
      LIMIT(181) = 165
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(51) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(61) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(68) = .TRUE.
      LTHRES(80) = .TRUE.
C
      RETURN
C
C1626 PRELIMINARY
 1626 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116)  = -1
      LIMIT(116) = 120
      KCONST(166)  = -1
      LIMIT(166) = 360
      KCONST(168)  = -1
      LIMIT(168) = 150
      KCONST(185)  = -1
      LIMIT(185) = 315
      KCONST(187)  = -1
      LIMIT(187) = 300
      KCONST(190)  = -1
      LIMIT(190) = 150
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(51) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(61) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(68) = .TRUE.
C
      RETURN
C
      END
