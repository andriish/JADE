C   24/07/79 C9082701   MEMBER NAME  ANOM12   (S)           FORTRAN
      SUBROUTINE ANOM12(IRUN)
C---  ANOMALIES OF RUNS 764 - 893
C---  15.8 GEV      JULY 1979
C
C     H.WRIEDT    24.07.79     23:55
C     LAST MODIFICATION     27.08.79     12:00
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
      GOTO (764,765,13,767,768,13,13,13,13,773,13,775,776,13,778,13,13,
     &      13,13,13,784,785,786,13,788,13,13,13,13,793,794,795,796,13,
     &      798,13,13,13,13,13,804,13,806,13,13,809,13,13,13,13,13,
     &      815,816,817,13,819,820,13,822,13,824,825,826,13,828,
     &      829,830,13,13,833,13,835,13,837,838,839,840,13,13,13,
     &      844,845,846,13,13,13,850,13,13,13,13,855,13,13,13,13,
     &      860,13,13,863,13,865,866,867,13,869,870,13,13,13,874,
     &      875,13,13,13,879,880,13,882,883,13,13,886,887,888,889,
     &      890,891,892,893),IRUN
   13 RETURN
C
  764 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(186) = .TRUE.
C
      RETURN
C
  765 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(185) = -1
      LIMIT(185) = 300
C
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  767 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(158) = -1
      LIMIT(158) = 285
      KCONST(161) = -1
      LIMIT(161) = 270
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(63) = .TRUE.
      LTHRES(97) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  768 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(112) = .TRUE.
C
      RETURN
C
  773 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(158) = -1
      LIMIT(158) = 270
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  775 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
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
      KCONST(185) = -1
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  776 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
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
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  778 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(158) = -1
      LIMIT(158) = 270
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  784 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(147) = -1
      LIMIT(147) = 270
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
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*785 PRELIMINARY
  785 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(147) = -1
      LIMIT(147) = 270
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
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*786 PRELIMINARY
  786 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(147) = -1
      LIMIT(147) = 270
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
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*788 PRELIMINARY
  788 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(147) = -1
      LIMIT(147) = 270
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
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*793 PRELIMINARY
  793 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(147) = -1
      LIMIT(147) = 270
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
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  794 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(23) = -1
      LIMIT(23) = 135
      KCONST(53) = -1
      LIMIT(53) = 180
      KCONST(59) = -1
      LIMIT(59) = 225
      KCONST(69) = -1
      LIMIT(69) = 135
      KCONST(84) = -1
      LIMIT(84) = 210
      KCONST(112) = -1
      LIMIT(112) = 30
      KCONST(116) = -1
      LIMIT(116) = 195
      KCONST(140) = -1
      LIMIT(140) = 375
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
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  795 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
      LCHANN(186) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 195
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(79) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  796 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
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
      KCONST(185) = -1
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(186) = .TRUE.
C
      RETURN
C
  798 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 375
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
      KCONST(185) = -1
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  804 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(158) = -1
      LIMIT(158) = 285
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(78) = .TRUE.
      LTHRES(79) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  806 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(158) = -1
      LIMIT(158) = 270
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  809 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(158) = -1
      LIMIT(158) = 270
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  815 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(148) = -1
      LIMIT(148) = 195
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
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(148) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  816 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 195
      KCONST(140) = -1
      LIMIT(140) = 375
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
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(34) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  817 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(158) = -1
      LIMIT(158) = 285
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(186) = .TRUE.
C
      RETURN
C
  819 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(162) = -1
      LIMIT(162) = 180
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(168) = -1
      LIMIT(168) = 240
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
      LTHRES(156) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  820 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 315
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 330
      KCONST(187) = -1
      LIMIT(187) = 315
C
      RETURN
C
  822 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
      KCONST(188) = -1
      LIMIT(188) = 120
C
      LTHRES(23) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(188) = .TRUE.
C
      RETURN
C
  824 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  825 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  826 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 195
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  828 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
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
      LIMIT(167) = 285
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  829 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
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
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*830 PRELIMINARY ***
  830 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 195
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(162) = -1
      LIMIT(162) = 195
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(168) = -1
      LIMIT(168) = 240
      KCONST(183) = -1
      LIMIT(183) = 240
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C*833 PRELIMINARY ***
  833 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 195
      KCONST(140) = -1
      LIMIT(140) = 375
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
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*835 PRELIMINARY ***
  835 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 195
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(162) = -1
      LIMIT(162) = 180
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(168) = -1
      LIMIT(168) = 240
      KCONST(183) = -1
      LIMIT(183) = 240
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
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
  837 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 165
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(162) = -1
      LIMIT(162) = 180
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(168) = -1
      LIMIT(168) = 240
      KCONST(183) = -1
      LIMIT(183) = 240
      KCONST(185) = -1
      LIMIT(185) = 300
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
      LTHRES(85) = .TRUE.
      LTHRES(89) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  838 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(153) = -1
      LIMIT(153) = 255
      KCONST(158) = -1
      LIMIT(158) = 285
      KCONST(161) = -1
      LIMIT(161) = 270
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 330
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  839 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(158) = -1
      LIMIT(158) = 285
      KCONST(161) = -1
      LIMIT(161) = 270
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*840 PRELIMINARY ***
  840 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(158) = -1
      LIMIT(158) = 285
      KCONST(161) = -1
      LIMIT(161) = 270
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*844 PRELIMINARY ***
  844 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(158) = -1
      LIMIT(158) = 285
      KCONST(161) = -1
      LIMIT(161) = 270
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C*845 PRELIMINARY ***
  845 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(158) = -1
      LIMIT(158) = 285
      KCONST(161) = -1
      LIMIT(161) = 270
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*846 PRELIMINARY ***
  846 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(158) = -1
      LIMIT(158) = 285
      KCONST(161) = -1
      LIMIT(161) = 270
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*850 PRELIMINARY ***
  850 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(158) = -1
      LIMIT(158) = 285
      KCONST(161) = -1
      LIMIT(161) = 270
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*855 PRELIMINARY ***
  855 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(158) = -1
      LIMIT(158) = 285
      KCONST(161) = -1
      LIMIT(161) = 270
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  860 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*863 PRELIMINARY ***
  863 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*865 PRELIMINARY ***
  865 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*866 PRELIMINARY ***
  866 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*867 PRELIMINARY ***
  867 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  869 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(167) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*870 PRELIMINARY ***
  870 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(167) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*874 PRELIMINARY ***
  874 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(167) = -1
      LIMIT(167) = 285
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(167) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  875 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(158) = -1
      LIMIT(158) = 270
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*879 PRELIMINARY ***
  879 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(158) = -1
      LIMIT(158) = 270
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*880 PRELIMINARY ***
  880 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(158) = -1
      LIMIT(158) = 270
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  882 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(158) = -1
      LIMIT(158) = 285
      KCONST(162) = -1
      LIMIT(162) = 180
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
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
      LTHRES(82) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*883 CONTINUE
  883 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(158) = -1
      LIMIT(158) = 285
      KCONST(162) = -1
      LIMIT(162) = 180
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
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
      LTHRES(82) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*886 PRELIMINARY
  886 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(158) = -1
      LIMIT(158) = 285
      KCONST(162) = -1
      LIMIT(162) = 180
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
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
      LTHRES(82) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*887 PRELIMINARY
  887 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(158) = -1
      LIMIT(158) = 285
      KCONST(162) = -1
      LIMIT(162) = 180
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
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
      LTHRES(82) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  888 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(158) = -1
      LIMIT(158) = 270
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*889 PRELIMINARY
  889 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(158) = -1
      LIMIT(158) = 270
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*890 PRELIMINARY
  890 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(158) = -1
      LIMIT(158) = 270
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*891 PRELIMINARY
  891 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(158) = -1
      LIMIT(158) = 270
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*892 PRELIMINARY
  892 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(158) = -1
      LIMIT(158) = 270
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*893 PRELIMINARY
  893 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(146) = -1
      LIMIT(146) = 270
      KCONST(147) = -1
      LIMIT(147) = 270
      KCONST(149) = -1
      LIMIT(149) = 300
      KCONST(150) = -1
      LIMIT(150) = 285
      KCONST(158) = -1
      LIMIT(158) = 270
      KCONST(164) = -1
      LIMIT(164) = 270
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
      END
