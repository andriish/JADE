C   24/07/79 C9072401   MEMBER NAME  ANOM11   (S)           FORTRAN
      SUBROUTINE ANOM11(IRUN)
C---  ANOMALIES OF RUNS 699 - 709
C---  15.8 GEV     JULY 1979
C
C     H.WRIEDT    24.07.79     23:50
C     LAST MODIFICATION     24.07.79     23:50
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
      GOTO (699,12,701,12,12,704,705,706,707,12,709),IRUN
   12 RETURN
C
  699 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 345
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 270
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 300
C
      RETURN
C
  701 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 345
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 300
      KCONST(190) = -1
      LIMIT(190) = 225
C
      RETURN
C
  704 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
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
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 300
C
      RETURN
C
  705 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
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
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 300
C
      RETURN
C
  706 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 195
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
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 300
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(53) = .TRUE.
      LTHRES(85) = .TRUE.
C
      RETURN
C
  707 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
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
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 300
C
      RETURN
C
  709 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 345
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 270
      KCONST(162) = -1
      LIMIT(162) = 180
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(168) = -1
      LIMIT(168) = 255
      KCONST(178) = -1
      LIMIT(178) = 285
      KCONST(183) = -1
      LIMIT(183) = 240
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 300
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(53) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(171) = .TRUE.
C
      RETURN
C
      END
