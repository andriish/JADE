C   08/08/79 C9090501   MEMBER NAME  ANOM31   (S)           FORTRAN
      SUBROUTINE ANOM31(IRUN)
C---  ANOMALIES OF RUNS 1084 - 1121
C---  13.86 GEV      JULY 1979
C
C     H.WRIEDT    27.07.79     02:20
C     LAST MODIFICATION     05.09.79     13:00
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
      GOTO (1084,1085,1086,1087,1088,1089,1090,1091,1092,1093,13,1095,
     &      1096,1097,1098,1099,13,13,1102,1103,1104,1105,1106,1107,13,
     &      13,13,13,13,13,1114,1115,1116,1117,1118,1119,1120,1121),IRUN
   13 RETURN
C
 1084 LCHANN(79) = .TRUE.
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
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
C
      RETURN
C
C1085 PRELIMINARY
 1085 LCHANN(79) = .TRUE.
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
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
C
      RETURN
C
C1086 PRELIMINARY
 1086 LCHANN(79) = .TRUE.
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
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
C
      RETURN
C
C1087 PRELIMINARY
 1087 LCHANN(79) = .TRUE.
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
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
C
      RETURN
C
C1088 PRELIMINARY
 1088 LCHANN(79) = .TRUE.
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
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
 1089 LCHANN(79) = .TRUE.
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
      LTHRES(67) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1090 PRELIMINARY
 1090 LCHANN(79) = .TRUE.
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
      LTHRES(67) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1091 PRELIMINARY
 1091 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(162) = -1
      LIMIT(162) = 180
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(168) = -1
      LIMIT(168) = 240
      KCONST(183) = -1
      LIMIT(183) = 240
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(92) = .TRUE.
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
C1092 PRELIMINARY
 1092 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(67) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1093 PRELIMINARY
 1093 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(67) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 1095 LCHANN(79) = .TRUE.
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
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1096 PRELIMINARY
 1096 LCHANN(79) = .TRUE.
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
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1097 PRELIMINARY
 1097 LCHANN(79) = .TRUE.
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
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1098 PRELIMINARY
 1098 LCHANN(79) = .TRUE.
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
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 1099 LCHANN(79) = .TRUE.
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
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 1102 LCHANN(79) = .TRUE.
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
      KCONST(168) = -1
      LIMIT(168) = 350
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(185) = .TRUE.
C
      RETURN
C
C1103 PRELIMINARY
 1103 LCHANN(79) = .TRUE.
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
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(183) = .TRUE.
C
      RETURN
C
C1104 PRELIMINARY
 1104 LCHANN(79) = .TRUE.
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
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(183) = .TRUE.
C
      RETURN
C
C1105 PRELIMINARY
 1105 LCHANN(79) = .TRUE.
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
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(183) = .TRUE.
C
      RETURN
C
 1106 LCHANN(79) = .TRUE.
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
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(7) = .TRUE.
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
 1107 LCHANN(79) = .TRUE.
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
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 1114 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(67) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1115 PRELIMINARY
 1115 LCHANN(79) = .TRUE.
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
      LTHRES(67) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1116 PRELIMINARY
 1116 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 165
      KCONST(162) = -1
      LIMIT(162) = 180
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(168) = -1
      LIMIT(168) = 240
      KCONST(183) = -1
      LIMIT(183) = 240
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(67) = .TRUE.
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
C1117 PRELIMINARY
 1117 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(67) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1118 PRELIMINARY
 1118 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(67) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1119 PRELIMINARY
 1119 LCHANN(79) = .TRUE.
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
      LTHRES(67) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 1120 LCHANN(79) = .TRUE.
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
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1121 PRELIMINARY
 1121 LCHANN(79) = .TRUE.
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
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
      END
