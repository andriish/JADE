C   27/07/79 C9090501   MEMBER NAME  ANOM14   (S)           FORTRAN
      SUBROUTINE ANOM14(IRUN)
C---  ANOMALIES OF RUNS 972 - 1082
C---  15.8 GEV      JULY 1979
C
C     H.WRIEDT    27.07.79     02:20
C     LAST MODIFICATION     05.09.79     12:55
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
      GOTO (972,13,974,975,976,13,13,979,980,13,13,
     +      983,13,13,986,987,13,13,13,13,992,993,13,13,996,997,13,
     +      999,13,13,13,13,13,1005,13,1007,1008,13,1010,1011,1012,
     +      1013,1014,13,1016,1017,1018,1019,13,13,1022,1023,1024,13,
     +      1026,13,1028,13,13,13,1032,1033,13,1035,1036,13,13,13,13,
     +      1041,1042,13,1044,13,13,1047,13,1049,1050,1051,1052,13,
     +      1054,13,1056,13,13,13,1060,13,13,13,13,13,13,13,13,13,13,
     +      1071,1072,13,13,13,1076,1077,1078,1079,1080,1081,1082),IRUN
   13 RETURN
C
  972 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*974 PRELIMINARY
  974 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*975 PRELIMINARY
  975 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*976 PRELIMINARY
  976 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*979 PRELIMINARY
  979 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 195
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*980 PRELIMINARY
  980 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 195
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  983 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(23) = -1
      LIMIT(23) = 75
      KCONST(67) = -1
      LIMIT(67) = 150
      KCONST(78) = -1
      LIMIT(78) = 105
      KCONST(79) = -1
      LIMIT(79) = 345
      KCONST(107) = -1
      LIMIT(107) = 300
      KCONST(108) = -1
      LIMIT(108) = 195
      KCONST(112) = -1
      LIMIT(112) = 30
      KCONST(114) = -1
      LIMIT(114) = 165
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(120) = -1
      LIMIT(120) = 240
      KCONST(122) = -1
      LIMIT(122) = 135
      KCONST(128) = -1
      LIMIT(128) = 120
      KCONST(129) = -1
      LIMIT(129) = 120
      KCONST(132) = -1
      LIMIT(132) = 225
      KCONST(140) = -1
      LIMIT(140) = 375
      KCONST(151) = -1
      LIMIT(151) = 255
      KCONST(156) = -1
      LIMIT(156) = 150
      KCONST(162) = -1
      LIMIT(162) = 180
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(171) = -1
      LIMIT(171) = 165
      KCONST(173) = -1
      LIMIT(173) = 435
      KCONST(175) = -1
      LIMIT(175) = 210
      KCONST(180) = -1
      LIMIT(180) = 105
      KCONST(181) = -1
      LIMIT(181) = 180
      KCONST(182) = -1
      LIMIT(182) = 150
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
      KCONST(188) = -1
      LIMIT(188) = 120
C
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(110) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(126) = .TRUE.
      LTHRES(145) = .TRUE.
      LTHRES(150) = .TRUE.
      LTHRES(159) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(165) = .TRUE.
      LTHRES(170) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(174) = .TRUE.
      LTHRES(176) = .TRUE.
      LTHRES(178) = .TRUE.
      LTHRES(186) = .TRUE.
C
      RETURN
C
  986 LCHANN(79) = .TRUE.
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
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C*987 PRELIMINARY
  987 LCHANN(79) = .TRUE.
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
      LTHRES(23) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  992 LCHANN(79) = .TRUE.
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
      LTHRES(11) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(27) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(184) = .TRUE.
C
      RETURN
C
  993 LCHANN(79) = .TRUE.
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
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  996 LCHANN(79) = .TRUE.
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
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  997 LCHANN(79) = .TRUE.
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
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
  999 LCHANN(79) = .TRUE.
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
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1005 PRELIMINARY
 1005 LCHANN(79) = .TRUE.
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
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 1007 LCHANN(79) = .TRUE.
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
      LTHRES(25) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1008 PRELIMINARY
 1008 LCHANN(79) = .TRUE.
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
      LTHRES(25) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1010 PRELIMINARY
 1010 LCHANN(79) = .TRUE.
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
      LTHRES(25) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1011 PRELIMINARY
 1011 LCHANN(79) = .TRUE.
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
      LTHRES(25) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1012 PRELIMINARY
 1012 LCHANN(79) = .TRUE.
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
      LTHRES(25) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1013 PRELIMINARY
 1013 LCHANN(79) = .TRUE.
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
      LTHRES(25) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 1014 LCHANN(79) = .TRUE.
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
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1016 PRELIMINARY
 1016 LCHANN(79) = .TRUE.
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
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1017 PRELIMINARY
 1017 LCHANN(79) = .TRUE.
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
      LTHRES(25) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 1018 LCHANN(79) = .TRUE.
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
      LIMIT(149) = 315
      KCONST(150) = -1
      LIMIT(150) = 300
      KCONST(162) = -1
      LIMIT(162) = 165
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
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1019 PRELIMINARY
 1019 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
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
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1022 PRELIMINARY
 1022 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(140) = -1
      LIMIT(140) = 375
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
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 1023 LCHANN(79) = .TRUE.
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
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1024 PRELIMINARY
 1024 LCHANN(79) = .TRUE.
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
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1026 PRELIMINARY
 1026 LCHANN(79) = .TRUE.
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
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 1028 LCHANN(79) = .TRUE.
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
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1032 PRELIMINARY
 1032 LCHANN(79) = .TRUE.
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
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
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
C1033 PRELIMINARY
 1033 LCHANN(79) = .TRUE.
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
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1035 PRELIMINARY
 1035 LCHANN(79) = .TRUE.
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
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1036 PRELIMINARY
 1036 LCHANN(79) = .TRUE.
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
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 1041 LCHANN(79) = .TRUE.
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
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1042 PRELIMINARY
 1042 LCHANN(79) = .TRUE.
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
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1044 PRELIMINARY
 1044 LCHANN(79) = .TRUE.
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
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1047 PRELIMINARY
 1047 LCHANN(79) = .TRUE.
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
      KCONST(162) = -1
      LIMIT(162) = 165
      KCONST(164) = -1
      LIMIT(164) = 285
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
 1049 LCHANN(79) = .TRUE.
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
      LTHRES(25) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1050 PRELIMINARY
 1050 LCHANN(79) = .TRUE.
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
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1051 PRELIMINARY
 1051 LCHANN(79) = .TRUE.
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
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
 1052 LCHANN(79) = .TRUE.
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
      LIMIT(150) = 285
      KCONST(162) = -1
      LIMIT(162) = 165
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(167) = -1
      LIMIT(167) = 300
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(183) = -1
      LIMIT(183) = 240
      KCONST(185) = -1
      LIMIT(185) = 315
      KCONST(187) = -1
      LIMIT(187) = 315
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1054 PRELIMINARY
 1054 LCHANN(79) = .TRUE.
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
      LIMIT(150) = 285
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
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1056 PRELIMINARY
 1056 LCHANN(79) = .TRUE.
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
C
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1060 PRELIMINARY
 1060 LCHANN(79) = .TRUE.
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
      LIMIT(150) = 285
      KCONST(162) = -1
      LIMIT(162) = 165
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
      LTHRES(7) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
 1071 LCHANN(79) = .TRUE.
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
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1072 PRELIMINARY
 1072 LCHANN(79) = .TRUE.
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
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1076 PRELIMINARY
 1076 LCHANN(79) = .TRUE.
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
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1077 PRELIMINARY
 1077 LCHANN(79) = .TRUE.
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
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1078 PRELIMINARY
 1078 LCHANN(79) = .TRUE.
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
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 1079 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 1080 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(49) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1081 PRELIMINARY
 1081 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(25) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1082 PRELIMINARY
 1082 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 165
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
      END
