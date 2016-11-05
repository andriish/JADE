C   09/10/79 911301222  MEMBER NAME  ANOM63   (S)           FORTRAN
      SUBROUTINE ANOM63(IRUN)
C---  ANOMALIES OF RUNS 1578 - 1604
C---  15.27 GEV      SEPTEMBER 1979
C
C     H.WRIEDT    27.07.79     02:20
C     LAST MODIFICATION     30.11.79     12:00
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
      GOTO (1578,1579,13,13,13,13,13,13,13,13,13,13,13,1591,1592,1593,
     *      1594,13,1596,1597,13,1599,1600,1601,1602,1603,1604),IRUN
   13 RETURN
C
 1578 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
C
      RETURN
C
 1579 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
C
      RETURN
C
 1591 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(97) = -1
      LIMIT(97) = 240
      KCONST(117) = -1
      LIMIT(117) = 75
      KCONST(118) = -1
      LIMIT(118) = 150
      KCONST(145) = -1
      LIMIT(145) = 90
      KCONST(148) = -1
      LIMIT(148) = 105
      KCONST(150) = -1
      LIMIT(150) = 345
      KCONST(151) = -1
      LIMIT(151) = 180
      KCONST(153) = -1
      LIMIT(153) = 300
      KCONST(155) = -1
      LIMIT(155) = 240
      KCONST(166) = -1
      LIMIT(166) = 420
      KCONST(169) = -1
      LIMIT(169) = 75
      KCONST(171) = -1
      LIMIT(171) = 150
      KCONST(185) = -1
      LIMIT(185) = 360
C
      LTHRES(118) = .TRUE.
      LTHRES(177) = .TRUE.
C
      RETURN
C
 1592 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
        DO 15920 I = 97,142
15920   KCONST(I) = -10
      KCONST(97) = -1
      LIMIT(97) = 285
      KCONST(101) = -1
      LIMIT(101) = 150
      KCONST(166) = -1
      LIMIT(166) = 435
      KCONST(185) = -1
      LIMIT(185) = 360
C
      LTHRES(27) = .TRUE.
      LTHRES(97) = .TRUE.
      LTHRES(101) = .TRUE.
        DO 15921 I = 145,190
15921   LTHRES(I) = .TRUE.
C
      RETURN
C
 1593 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
        DO 15930 I = 97,142
15930   KCONST(I) = -10
      KCONST(166) = -1
      LIMIT(166) = 435
      KCONST(185) = -1
      LIMIT(185) = 375
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(51) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(61) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(68) = .TRUE.
      LTHRES(74) = .TRUE.
      LTHRES(79) = .TRUE.
      LTHRES(80) = .TRUE.
        DO 15931 I = 145,190
15931   LTHRES(I) = .TRUE.
C
      RETURN
C
 1594 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
        DO 15940 I = 97,142
15940   KCONST(I) = -10
      KCONST(146) = -1
      LIMIT(146) = 330
      KCONST(150) = -1
      LIMIT(150) = 330
      KCONST(158) = -1
      LIMIT(158) = 345
      KCONST(164) = -1
      LIMIT(164) = 345
      KCONST(166) = -1
      LIMIT(166) = 435
      KCONST(167) = -1
      LIMIT(167) = 330
      KCONST(185) = -1
      LIMIT(185) = 375
C
      RETURN
C
 1596 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(154) = -1
      LIMIT(154) = 45
      KCONST(166) = -1
      LIMIT(166) = 420
      KCONST(185) = -1
      LIMIT(185) = 360
C
      LTHRES(49) = .TRUE.
      LTHRES(154) = .TRUE.
C
      RETURN
C
 1597 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 420
C
      RETURN
C
 1599 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(97) = -1
      LIMIT(97) = 240
      KCONST(117) = -1
      LIMIT(117) = 60
      KCONST(166) = -1
      LIMIT(166) = 420
      KCONST(185) = -1
      LIMIT(185) = 360
C
      LTHRES(154) = .TRUE.
C
      RETURN
C
 1600 LCHANN(49) = .TRUE.
      LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
        DO 16000 I = 97,103
16000   KCONST(I) = -10
      KCONST(123) = -10
      KCONST(125) = -10
      KCONST(130) = -10
      KCONST(133) = -10
      KCONST(134) = -10
      KCONST(140) = -10
      KCONST(55) = -1
      LIMIT(55) = 45
      KCONST(67) = -1
      LIMIT(67) = 30
      KCONST(166) = -1
      LIMIT(166) = 435
      KCONST(185) = -1
      LIMIT(185) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(51) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(61) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(68) = .TRUE.
      LTHRES(74) = .TRUE.
      LTHRES(75) = .TRUE.
      LTHRES(80) = .TRUE.
C
      RETURN
C
 1601 LCHANN(49) = .TRUE.
      LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
        DO 16010 I = 97,142
16010   KCONST(I) = -10
      KCONST(55) = -1
      LIMIT(55) = 45
      KCONST(67) = -1
      LIMIT(67) = 30
      KCONST(166) = -1
      LIMIT(166) = 450
      KCONST(185) = -1
      LIMIT(185) = 375
C
      LTHRES(7) = .TRUE.
      LTHRES(51) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(61) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(68) = .TRUE.
      LTHRES(80) = .TRUE.
      LTHRES(146) = .TRUE.
      LTHRES(150) = .TRUE.
      LTHRES(158) = .TRUE.
      LTHRES(164) = .TRUE.
      LTHRES(167) = .TRUE.
C
      RETURN
C
 1602 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
        DO 16020 I = 97,142
16020   KCONST(I) = -10
      KCONST(151) = -1
      LIMIT(151) = 180
      KCONST(158) = -1
      LIMIT(158) = 345
      KCONST(164) = -1
      LIMIT(164) = 345
      KCONST(166) = -1
      LIMIT(166) = 435
      KCONST(185) = -1
      LIMIT(185) = 360
C
      LTHRES(51) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(61) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(68) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(80) = .TRUE.
        DO 16021 I = 145,190
16021   LTHRES(I) = .TRUE.
C
      RETURN
C
 1603 LCHANN(49) = .TRUE.
      LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
        DO 16030 I = 97,142
16030   KCONST(I) = -10
      KCONST(145) = -1
      LIMIT(145) = 90
      KCONST(151) = -1
      LIMIT(151) = 195
      KCONST(164) = -1
      LIMIT(164) = 330
      KCONST(166) = -1
      LIMIT(166) = 435
      KCONST(169) = -1
      LIMIT(169) = 90
      KCONST(185) = -1
      LIMIT(185) = 360
C
        DO 16031 I = 145,190
16031   LTHRES(I) = .TRUE.
C
      RETURN
C
 1604 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(7) = -1
      LIMIT(7) = 45
      KCONST(49) = -1
      LIMIT(49) = 90
      KCONST(51) = -1
      LIMIT(51) = 75
      KCONST(55) = -1
      LIMIT(55) = 90
      KCONST(61) = -1
      LIMIT(61) = 105
      KCONST(67) = -1
      LIMIT(67) = 90
      KCONST(68) = -1
      LIMIT(68) = 60
      KCONST(74) = -1
      LIMIT(74) = 135
      KCONST(97) = -1
      LIMIT(97) = 255
      KCONST(98) = -1
      LIMIT(98) = 390
      KCONST(99) = -1
      LIMIT(99) = 330
      KCONST(100) = -1
      LIMIT(100) = 360
      KCONST(103) = -1
      LIMIT(103) = 300
      KCONST(105) = -1
      LIMIT(105) = 270
      KCONST(110) = -1
      LIMIT(110) = 270
      KCONST(123) = -1
      LIMIT(123) = 390
      KCONST(125) = -1
      LIMIT(125) = 330
      KCONST(130) = -1
      LIMIT(130) = 330
      KCONST(133) = -1
      LIMIT(133) = 360
      KCONST(134) = -1
      LIMIT(134) = 480
      KCONST(140) = -1
      LIMIT(140) = 510
      KCONST(142) = -1
      LIMIT(142) = 270
      KCONST(146) = -1
      LIMIT(146) = 390
      KCONST(147) = -1
      LIMIT(147) = 360
      KCONST(150) = -1
      LIMIT(150) = 390
      KCONST(158) = -1
      LIMIT(158) = 390
      KCONST(161) = -1
      LIMIT(161) = 390
      KCONST(164) = -1
      LIMIT(164) = 390
      KCONST(166) = -1
      LIMIT(166) = 435
      KCONST(167) = -1
      LIMIT(167) = 390
      KCONST(185) = -1
      LIMIT(185) = 420
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(51) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(61) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(68) = .TRUE.
      LTHRES(74) = .TRUE.
      LTHRES(80) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(90) = .TRUE.
C
      RETURN
C
      END
