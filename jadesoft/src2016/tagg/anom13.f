C   25/07/79 C9082701   MEMBER NAME  ANOM13   (S)           FORTRAN
      SUBROUTINE ANOM13(IRUN)
C---  ANOMALIES OF RUNS 914 - 960
C---  15.8 GEV      JULY 1979
C
C     H.WRIEDT    24.07.79     23:55
C     LAST MODIFICATION     27.08.79     12:20
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
      GOTO (914,915,916,13,918,919,13,13,13,13,13,13,926,13,13,13,
     &      930,931,13,13,934,13,936,937,13,13,13,13,13,13,944,13,946,
     &      13,13,13,950,951,13,13,13,13,956,957,958,13,960),IRUN
   13 RETURN
C
  914 LCHANN(79) = .TRUE.
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
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  915 LCHANN(79) = .TRUE.
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
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*916 CONTINUE
  916 LCHANN(79) = .TRUE.
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
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*918 CONTINUE
  918 LCHANN(79) = .TRUE.
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
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  919 LCHANN(79) = .TRUE.
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
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(185) = -1
      LIMIT(185) = 300
      KCONST(187) = -1
      LIMIT(187) = 315
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C*926 CONTINUE
  926 LCHANN(79) = .TRUE.
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
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  930 LCHANN(79) = .TRUE.
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
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*931 PRELIMINARY ***
  931 LCHANN(79) = .TRUE.
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
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*934 PRELIMINARY ***
  934 LCHANN(79) = .TRUE.
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
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*936 PRELIMINARY ***
  936 LCHANN(79) = .TRUE.
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
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*937 PRELIMINARY ***
  937 LCHANN(79) = .TRUE.
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
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*944 PRELIMINARY ***
  944 LCHANN(79) = .TRUE.
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
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  946 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(59) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*950 PRELIMINARY
  950 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(59) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*951 PRELIMINARY
  951 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(59) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*956 PRELIMINARY
  956 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(59) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*957 PRELIMINARY
  957 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(59) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*958 PRELIMINARY
  958 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(59) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*960 PRELIMINARY
  960 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(59) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
      END
