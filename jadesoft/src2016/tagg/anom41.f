C   08/08/79 C9091901   MEMBER NAME  ANOM41   (S)           FORTRAN
      SUBROUTINE ANOM41(IRUN)
C---  ANOMALIES OF RUNS 1122 - 1162
C---  11.0 GEV      JULY 1979
C
C     H.WRIEDT    27.07.79     02:20
C     LAST MODIFICATION     19.09.79     13:25
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
      GOTO (1122,1123,1124,1125,1126,1127,1128,1129,1130,1131,1132,1133,
     +      1134,1135,1136,1137,13,1139,1140,13,1142,13,1144,1145,1146,
     +      1147,13,1149,1150,1151,1152,1153,1154,1155,1156,1157,13,
     +      1159,1160,1161,1162),IRUN
   13 RETURN
C
C1122 PRELIMINARY
 1122 LCHANN(79) = .TRUE.
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
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1123 PRELIMINARY
 1123 LCHANN(79) = .TRUE.
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
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1124 PRELIMINARY
 1124 LCHANN(79) = .TRUE.
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
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1125 PRELIMINARY
 1125 LCHANN(79) = .TRUE.
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
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1126 PRELIMINARY
 1126 LCHANN(79) = .TRUE.
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
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1127 PRELIMINARY
 1127 LCHANN(79) = .TRUE.
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
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1128 PRELIMINARY
 1128 LCHANN(79) = .TRUE.
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
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1129 PRELIMINARY
 1129 LCHANN(79) = .TRUE.
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
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 1130 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(27) = -1
      LIMIT(27) = 165
      KCONST(43) = -1
      LIMIT(43) = 225
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(162) = -1
      LIMIT(162) = 165
      KCONST(165) = -1
      LIMIT(165) = 210
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(183) = -1
      LIMIT(183) = 210
      KCONST(190) = -1
      LIMIT(190) = 210
C
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1131 PRELIMINARY
 1131 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(27) = -1
      LIMIT(27) = 165
      KCONST(43) = -1
      LIMIT(43) = 225
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(162) = -1
      LIMIT(162) = 165
      KCONST(165) = -1
      LIMIT(165) = 210
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(183) = -1
      LIMIT(183) = 210
      KCONST(190) = -1
      LIMIT(190) = 210
C
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1132 PRELIMINARY
 1132 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(27) = -1
      LIMIT(27) = 165
      KCONST(43) = -1
      LIMIT(43) = 225
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(162) = -1
      LIMIT(162) = 165
      KCONST(165) = -1
      LIMIT(165) = 210
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(183) = -1
      LIMIT(183) = 210
      KCONST(190) = -1
      LIMIT(190) = 210
C
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1133 PRELIMINARY
 1133 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(27) = -1
      LIMIT(27) = 165
      KCONST(43) = -1
      LIMIT(43) = 225
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(162) = -1
      LIMIT(162) = 165
      KCONST(165) = -1
      LIMIT(165) = 210
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(183) = -1
      LIMIT(183) = 210
      KCONST(190) = -1
      LIMIT(190) = 210
C
      LTHRES(49) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1134 PRELIMINARY
 1134 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(27) = -1
      LIMIT(27) = 165
      KCONST(43) = -1
      LIMIT(43) = 225
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(162) = -1
      LIMIT(162) = 165
      KCONST(165) = -1
      LIMIT(165) = 210
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(183) = -1
      LIMIT(183) = 210
      KCONST(190) = -1
      LIMIT(190) = 210
C
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1135 PRELIMINARY
 1135 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(27) = -1
      LIMIT(27) = 165
      KCONST(43) = -1
      LIMIT(43) = 225
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(162) = -1
      LIMIT(162) = 165
      KCONST(165) = -1
      LIMIT(165) = 210
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(183) = -1
      LIMIT(183) = 210
      KCONST(190) = -1
      LIMIT(190) = 210
C
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 1136 LCHANN(79) = .TRUE.
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
C1137 PRELIMINARY
 1137 LCHANN(79) = .TRUE.
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
C1139 PRELIMINARY
 1139 LCHANN(79) = .TRUE.
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
      LTHRES(72) = .TRUE.
      LTHRES(85) = .TRUE.
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
 1140 LCHANN(79) = .TRUE.
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
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1142 PRELIMINARY
 1142 LCHANN(79) = .TRUE.
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
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1144 PRELIMINARY
 1144 LCHANN(79) = .TRUE.
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
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1145 PRELIMINARY
 1145 LCHANN(79) = .TRUE.
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
      LTHRES(49) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1146 PRELIMINARY
 1146 LCHANN(79) = .TRUE.
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
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1147 PRELIMINARY
 1147 LCHANN(79) = .TRUE.
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
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 1149 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 165
      KCONST(162) = -1
      LIMIT(162) = 165
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(183) = -1
      LIMIT(183) = 210
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 1150 LCHANN(79) = .TRUE.
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
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1151 PRELIMINARY
 1151 LCHANN(79) = .TRUE.
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
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(6) = .TRUE.
      LTHRES(24) = .TRUE.
      LTHRES(30) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1152 PRELIMINARY
 1152 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1153 PRELIMINARY
 1153 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1154 PRELIMINARY
 1154 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 165
      KCONST(162) = -1
      LIMIT(162) = 165
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(183) = -1
      LIMIT(183) = 240
      KCONST(190) = -1
      LIMIT(190) = 225
C
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
C1155 PRELIMINARY
 1155 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 1156 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(162) = -1
      LIMIT(162) = 165
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
 1157 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(162) = -1
      LIMIT(162) = 165
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(49) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
 1159 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(162) = -1
      LIMIT(162) = 165
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
 1160 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(162) = -1
      LIMIT(162) = 165
      KCONST(166) = -1
      LIMIT(166) = 375
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
 1161 LCHANN(79) = .TRUE.
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
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
C1162 PRELIMINARY
 1162 LCHANN(79) = .TRUE.
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
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(106) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
      END
