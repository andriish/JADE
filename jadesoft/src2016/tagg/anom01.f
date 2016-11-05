C   24/07/79 C9082401   MEMBER NAME  ANOM01   (S)           FORTRAN
      SUBROUTINE ANOM01(IRUN)
C---  ANOMALIES OF RUNS 540 - 564
C---  13.6 GEV    JUNE 1979
C
C     H.WRIEDT    24.07.79     23:40
C     LAST MODIFICATION     24.08.79     11:25
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
      GOTO (540,541,10,543,544,545,10,547,10,549,10,10,552,553,10,10,
     &      10,10,10,559,560,561,10,563,564),IRUN
   10 RETURN
C
  540 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 330
C
      LTHRES(116) = .TRUE.
C
      RETURN
C
  541 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 330
      KCONST(149) = -1
      LIMIT(149) = 285
      KCONST(156) = -1
      LIMIT(156) = 285
      KCONST(166) = -1
      LIMIT(166) = 345
      KCONST(167) = -1
      LIMIT(167) = 270
      KCONST(171) = -1
      LIMIT(171) = 315
      KCONST(178) = -1
      LIMIT(178) = 300
      KCONST(185) = -1
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 285
C
      LTHRES(112) = .TRUE.
C
      RETURN
C
  543 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 330
C
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(186) = .TRUE.
C
      RETURN
C
  544 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 330
C
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  545 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 330
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(186) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
  547 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
      LCHANN(186) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 200
      KCONST(140) = -1
      LIMIT(140) = 330
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
  549 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
      LCHANN(186) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 330
C
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  552 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(162) = -1
      LIMIT(162) = 210
      KCONST(168) = -1
      LIMIT(168) = 240
      KCONST(190) = -1
      LIMIT(190) = 210
C
      LTHRES(53) = .TRUE.
      LTHRES(54) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(70) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(87) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(93) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(186) = .TRUE.
C
      RETURN
C
  553 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
      LCHANN(186) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(190) = -1
      LIMIT(190) = 210
C
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(170) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(186) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
  559 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      LTHRES(52) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  560 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 330
C
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  561 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(149) = -1
      LIMIT(149) = 285
      KCONST(166) = -1
      LIMIT(166) = 345
      KCONST(167) = -1
      LIMIT(167) = 270
      KCONST(178) = -1
      LIMIT(178) = 300
      KCONST(185) = -1
      LIMIT(185) = 285
      KCONST(187) = -1
      LIMIT(187) = 285
C
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
C
      RETURN
C
  563 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(162) = -1
      LIMIT(162) = 210
      KCONST(168) = -1
      LIMIT(168) = 240
      KCONST(190) = -1
      LIMIT(190) = 210
C
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  564 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      LTHRES(59) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
C
      RETURN
C
      END
