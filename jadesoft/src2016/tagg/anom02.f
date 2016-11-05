C   24/07/79 C9082301   MEMBER NAME  ANOM02   (S)           FORTRAN
      SUBROUTINE ANOM02(IRUN)
C---  ANOMALIES OF RUNS 581 - 672
C---  13.86 GEV     JUNE 1979
C
C     H.WRIEDT    24.07.79     23:45
C     LAST MODIFICATION     23.08.79     15:15
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
      GOTO (581,582,11,584,11,586,587,588,589,590,591,592,593,594,595,
     +      596,597,598,599,600,601,602,603,604,605,606,607,608,609,
     +      610,611,612,613,614,11,616,617,618,619,620,621,622,623,624,
     +      11,11,11,11,11,630,631,632,633,634,635,636,637,638,639,640,
     +      641,642,643,644,645,646,647,648,649,650,651,11,11,654,655,
     +      656,657,658,659,660,661,662,663,664,665,666,667,668,669,
     +      670,671,672),IRUN
   11 RETURN
C
  581 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
C
      LTHRES(59) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  582 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 210
C
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  584 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 165
      KCONST(162) = -1
      LIMIT(162) = 195
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(183) = -1
      LIMIT(183) = 225
      KCONST(190) = -1
      LIMIT(190) = 195
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(54) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(86) = .TRUE.
      LTHRES(87) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(97) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  586 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 300
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  587 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 330
C
      LTHRES(23) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  588 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(33) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(129) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  589 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  590 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 210
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(162) = -1
      LIMIT(162) = 210
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(173) = -1
      LIMIT(173) = 375
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(54) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(87) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(93) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  591 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 210
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(162) = -1
      LIMIT(162) = 210
      KCONST(168) = -1
      LIMIT(168) = 255
      KCONST(190) = -1
      LIMIT(190) = 240
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
  592 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(79) = +1
      LIMIT(79) = 390
      KCONST(140) = -1
      LIMIT(140) = 345
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(11) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(27) = .TRUE.
      LTHRES(33) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(97) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
  593 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(162) = -1
      LIMIT(162) = 210
      KCONST(168) = -1
      LIMIT(168) = 240
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(23) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
  594 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  595 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  596 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 210
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  597 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 210
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  598 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 210
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(162) = -1
      LIMIT(162) = 210
      KCONST(168) = -1
      LIMIT(168) = 270
      KCONST(190) = -1
      LIMIT(190) = 240
C
      LTHRES(23) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
  599 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  600 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 210
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  601 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(33) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  602 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 210
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
  603 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 165
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(162) = -1
      LIMIT(162) = 195
      KCONST(168) = -1
      LIMIT(168) = 240
      KCONST(183) = -1
      LIMIT(183) = 255
      KCONST(190) = -1
      LIMIT(190) = 210
C
      LTHRES(21) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(87) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(93) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  604 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  605 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(21) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  606 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  607 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 195
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  608 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  609 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 195
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  610 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(190) = -1
      LIMIT(190) = 240
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  611 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(162) = -1
      LIMIT(162) = 180
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  612 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
C
      LTHRES(23) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
C
      RETURN
C
  613 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(53) = -1
      LIMIT(53) = 180
      KCONST(54) = -1
      LIMIT(54) = 225
      KCONST(58) = -1
      LIMIT(58) = 225
      KCONST(59) = -1
      LIMIT(59) = 210
      KCONST(63) = -1
      LIMIT(63) = 150
      KCONST(69) = -1
      LIMIT(69) = 120
      KCONST(72) = -1
      LIMIT(72) = 210
      KCONST(84) = -1
      LIMIT(84) = 180
      KCONST(85) = -1
      LIMIT(85) = 225
      KCONST(87) = -1
      LIMIT(87) = 180
      KCONST(116) = -1
      LIMIT(116) = 195
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(162) = -1
      LIMIT(162) = 210
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(11) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(27) = .TRUE.
      LTHRES(33) = .TRUE.
      LTHRES(52) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(54) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(62) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(70) = .TRUE.
      LTHRES(71) = .TRUE.
      LTHRES(76) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(81) = .TRUE.
      LTHRES(83) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(87) = .TRUE.
      LTHRES(88) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(90) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(92) = .TRUE.
      LTHRES(93) = .TRUE.
      LTHRES(105) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  614 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  616 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 210
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(162) = -1
      LIMIT(162) = 210
      KCONST(168) = -1
      LIMIT(168) = 255
      KCONST(183) = -1
      LIMIT(183) = 255
      KCONST(190) = -1
      LIMIT(190) = 240
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(87) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(93) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  617 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  618 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  619 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  620 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  621 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  622 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  623 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(85) = -1
      LIMIT(85) = 240
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(148) = -1
      LIMIT(148) = 165
      KCONST(149) = -1
      LIMIT(149) = 240
      KCONST(150) = -1
      LIMIT(150) = 270
      KCONST(164) = -1
      LIMIT(164) = 210
      KCONST(166) = -1
      LIMIT(166) = 300
      KCONST(167) = -1
      LIMIT(167) = 240
      KCONST(178) = -1
      LIMIT(178) = 300
      KCONST(185) = -1
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 300
C
      LTHRES(23) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(145) = .TRUE.
      LTHRES(148) = .TRUE.
      LTHRES(151) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(159) = .TRUE.
      LTHRES(165) = .TRUE.
      LTHRES(169) = .TRUE.
      LTHRES(170) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(174) = .TRUE.
      LTHRES(175) = .TRUE.
      LTHRES(176) = .TRUE.
      LTHRES(177) = .TRUE.
      LTHRES(180) = .TRUE.
      LTHRES(182) = .TRUE.
      LTHRES(186) = .TRUE.
      LTHRES(188) = .TRUE.
C
      RETURN
C
  624 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(145) = -1
      LIMIT(145) = 225
      KCONST(151) = -1
      LIMIT(151) = 255
      KCONST(159) = -1
      LIMIT(159) = 210
      KCONST(165) = -1
      LIMIT(165) = 195
      KCONST(170) = -1
      LIMIT(170) = 240
      KCONST(172) = -1
      LIMIT(172) = 135
      KCONST(174) = -1
      LIMIT(174) = 180
      KCONST(176) = -1
      LIMIT(176) = 150
      KCONST(177) = -1
      LIMIT(177) = 150
      KCONST(180) = -1
      LIMIT(180) = 120
      KCONST(182) = -1
      LIMIT(182) = 135
      KCONST(186) = -1
      LIMIT(186) = 165
      KCONST(188) = -1
      LIMIT(188) = 120
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(145) = .TRUE.
      LTHRES(151) = .TRUE.
      LTHRES(159) = .TRUE.
      LTHRES(165) = .TRUE.
      LTHRES(170) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(174) = .TRUE.
      LTHRES(176) = .TRUE.
      LTHRES(177) = .TRUE.
      LTHRES(180) = .TRUE.
      LTHRES(182) = .TRUE.
      LTHRES(186) = .TRUE.
      LTHRES(188) = .TRUE.
C
      RETURN
C
  630 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 195
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  631 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(162) = -1
      LIMIT(162) = 180
      KCONST(168) = -1
      LIMIT(168) = 240
      KCONST(190) = -1
      LIMIT(190) = 240
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(87) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(93) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  632 LCHANN(79) = .TRUE.
      LCHANN(105) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 345
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  633 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 210
      KCONST(131) = -1
      LIMIT(131) = 195
      KCONST(139) = -1
      LIMIT(139) = 180
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(145) = -1
      LIMIT(145) = 240
      KCONST(148) = -1
      LIMIT(148) = 195
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(151) = -1
      LIMIT(151) = 255
      KCONST(157) = -1
      LIMIT(157) = 255
      KCONST(158) = -1
      LIMIT(158) = 270
      KCONST(159) = -1
      LIMIT(159) = 210
      KCONST(165) = -1
      LIMIT(165) = 210
      KCONST(170) = -1
      LIMIT(170) = 255
      KCONST(172) = -1
      LIMIT(172) = 135
      KCONST(174) = -1
      LIMIT(174) = 180
      KCONST(176) = -1
      LIMIT(176) = 150
      KCONST(177) = -1
      LIMIT(177) = 150
      KCONST(180) = -1
      LIMIT(180) = 120
      KCONST(182) = -1
      LIMIT(182) = 150
      KCONST(186) = -1
      LIMIT(186) = 165
      KCONST(188) = -1
      LIMIT(188) = 135
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(131) = .TRUE.
      LTHRES(139) = .TRUE.
      LTHRES(145) = .TRUE.
      LTHRES(148) = .TRUE.
      LTHRES(149) = .TRUE.
      LTHRES(151) = .TRUE.
      LTHRES(157) = .TRUE.
      LTHRES(158) = .TRUE.
      LTHRES(159) = .TRUE.
      LTHRES(165) = .TRUE.
      LTHRES(170) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(174) = .TRUE.
      LTHRES(176) = .TRUE.
      LTHRES(177) = .TRUE.
      LTHRES(180) = .TRUE.
      LTHRES(182) = .TRUE.
      LTHRES(186) = .TRUE.
      LTHRES(188) = .TRUE.
C
      RETURN
C
  634 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  635 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 195
      KCONST(140) = -1
      LIMIT(140) = 345
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  636 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 345
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  637 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  638 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 345
C
      LTHRES(23) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  639 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 345
C
      LTHRES(23) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  640 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  641 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  642 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 210
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  643 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 210
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  644 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  645 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
  646 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  647 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
C
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  648 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(178) = -1
      LIMIT(178) = 315
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  649 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 345
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 270
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(178) = -1
      LIMIT(178) = 315
      KCONST(185) = -1
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 300
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  650 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 210
      KCONST(140) = -1
      LIMIT(140) = 345
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 270
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(178) = -1
      LIMIT(178) = 315
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 300
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  651 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 345
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(178) = -1
      LIMIT(178) = 300
      KCONST(185) = -1
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 300
C
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  654 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 345
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(178) = -1
      LIMIT(178) = 300
      KCONST(185) = -1
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 300
C
      LTHRES(23) = .TRUE.
      LTHRES(27) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  655 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(178) = -1
      LIMIT(178) = 315
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  656 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(178) = -1
      LIMIT(178) = 315
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  657 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 195
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 315
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(162) = -1
      LIMIT(162) = 210
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(168) = -1
      LIMIT(168) = 255
      KCONST(178) = -1
      LIMIT(178) = 315
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
      KCONST(190) = -1
      LIMIT(190) = 240
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
  658 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 195
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(178) = -1
      LIMIT(178) = 315
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  659 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(178) = -1
      LIMIT(178) = 315
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  660 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(178) = -1
      LIMIT(178) = 315
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  661 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(178) = -1
      LIMIT(178) = 315
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(93) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  662 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 195
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(178) = -1
      LIMIT(178) = 315
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  663 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(178) = -1
      LIMIT(178) = 315
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  664 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(178) = -1
      LIMIT(178) = 315
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  665 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 345
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 270
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(178) = -1
      LIMIT(178) = 315
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  666 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 345
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(178) = -1
      LIMIT(178) = 315
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(1) = .TRUE.
      LTHRES(20) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  667 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 345
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 270
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(178) = -1
      LIMIT(178) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 300
C
      LTHRES(1) = .TRUE.
      LTHRES(20) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  668 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 210
      KCONST(140) = -1
      LIMIT(140) = 345
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(162) = -1
      LIMIT(162) = 210
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(168) = -1
      LIMIT(168) = 255
      KCONST(178) = -1
      LIMIT(178) = 300
      KCONST(183) = -1
      LIMIT(183) = 255
      KCONST(185) = -1
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 300
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(1) = .TRUE.
      LTHRES(20) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(54) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(87) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  669 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 345
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(178) = -1
      LIMIT(178) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 300
C
      LTHRES(1) = .TRUE.
      LTHRES(20) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  670 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(149) = -1
      LIMIT(149) = 285
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(178) = -1
      LIMIT(178) = 300
      KCONST(185) = -1
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 300
C
      LTHRES(1) = .TRUE.
      LTHRES(20) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  671 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(178) = -1
      LIMIT(178) = 300
      KCONST(185) = -1
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 300
C
      LTHRES(1) = .TRUE.
      LTHRES(20) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(58) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
  672 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 360
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(178) = -1
      LIMIT(178) = 315
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 300
C
      LTHRES(1) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
C
      RETURN
C
      END
