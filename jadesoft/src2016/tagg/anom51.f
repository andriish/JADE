C   08/08/79 912121807  MEMBER NAME  ANOM51   (S)           FORTRAN
      SUBROUTINE ANOM51(IRUN)
C---  ANOMALIES OF RUNS 1210 - 1397
C---  15.0 GEV      AUGUST 1979
C
C     H.WRIEDT    27.07.79     02:20
C     LAST MODIFICATION     19.09.79     13:30
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
      GOTO (1210,1211,1212,1213,1214,1215,13,13,1218,13,13,1221,
     +      13,1223,1224,1225,1226,1227,1228,1229,1230,1231,1232,1233,
     +      13,13,13,13,1238,13,1240,13,1242,1243,1244,13,13,
     +      13,13,1249,13,1251,1252,13,1254,13,1256,1257,1258,1259,
     +      1260,1261,1262,1263,13,1265,1266,1267,1268,1269,1270,1271,
     +      1272,1273,1274,1275,1276,1277,13,13,1280,1281,1282,1283,13,
     +      13,1286,1287,13,1289,1290,1291,1292,1293,1294,1295,1296,
     +      1297,13,1299,1300,1301,1302,1303,1304,13,13,13,13,13,1310,
     +      1311,1312,1313,1314,1315,13,1317,1318,1319,1320,1321,13,13,
     +      13,13,1326,1327,1328,1329,1330,1331,13,13,13,1335,1336,1337,
     +      13,1339,1340,1341,13,13,1344,1345,1346,1347,1348,13,13,13,
     +      13,1353,13,13,13,1357,1358,1359,1360,1361,1362,13,1364,1365,
     +      1366,1367,1368,1369,13,1371,1372,1373,13,1375,1376,1377,
     +      1378,1379,1380,1381,1382,1383,1384,1385,1386,13,13,13,13,13,
     +      13,1393,13,1395,13,1397),IRUN
   13 RETURN
C
 1210 LCHANN(79) = .TRUE.
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
      RETURN
C
C1211 PRELIMINARY
 1211 LCHANN(79) = .TRUE.
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
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1212 PRELIMINARY
 1212 LCHANN(79) = .TRUE.
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
C
      RETURN
C
C1213 PRELIMINARY
 1213 LCHANN(79) = .TRUE.
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
C
      RETURN
C
C1214 PRELIMINARY
 1214 LCHANN(79) = .TRUE.
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
      RETURN
C
C1215 PRELIMINARY
 1215 LCHANN(79) = .TRUE.
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
      LTHRES(64) = .TRUE.
      LTHRES(71) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
 1218 LCHANN(79) = .TRUE.
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
      LTHRES(9) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(156) = .TRUE.
C
      RETURN
C
C1221 PRELIMINARY
 1221 LCHANN(79) = .TRUE.
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
      LTHRES(9) = .TRUE.
      LTHRES(156) = .TRUE.
C
      RETURN
C
C1223 PRELIMINARY
 1223 LCHANN(79) = .TRUE.
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
      LTHRES(156) = .TRUE.
C
      RETURN
C
C1224 PRELIMINARY
 1224 LCHANN(79) = .TRUE.
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
      LTHRES(156) = .TRUE.
C
      RETURN
C
C1225 PRELIMINARY
 1225 LCHANN(79) = .TRUE.
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
      LTHRES(49) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(64) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(70) = .TRUE.
      LTHRES(71) = .TRUE.
      LTHRES(77) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(92) = .TRUE.
      LTHRES(94) = .TRUE.
      LTHRES(151) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(165) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
 1226 LCHANN(79) = .TRUE.
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
        DO 12261 I = 97,190
12261   LTHRES(I) = .TRUE.
C
      RETURN
C
 1227 LCHANN(79) = .TRUE.
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
 1228 LCHANN(79) = .TRUE.
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
 1229 LCHANN(79) = .TRUE.
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
 1230 LCHANN(79) = .TRUE.
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
      LIMIT(168) = 210
      KCONST(183) = -1
      LIMIT(183) = 210
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
 1231 LCHANN(79) = .TRUE.
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
 1232 LCHANN(79) = .TRUE.
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
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
 1233 LCHANN(79) = .TRUE.
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
      LIMIT(168) = 210
      KCONST(183) = -1
      LIMIT(183) = 210
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
 1238 LCHANN(79) = .TRUE.
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
 1240 LCHANN(79) = .TRUE.
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
      LIMIT(166) = 360
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(25) = .TRUE.
      LTHRES(79) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1242 PRELIMINARY
 1242 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(25) = .TRUE.
      LTHRES(79) = .TRUE.
      LTHRES(112) = .TRUE.
C
      RETURN
C
C1243 PRELIMINARY
 1243 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(25) = .TRUE.
      LTHRES(79) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
C1244 PRELIMINARY
 1244 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(25) = .TRUE.
      LTHRES(79) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
 1249 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(7) = +1
      LIMIT(7) = 315
      KCONST(112) = +1
      LIMIT(112) = 105
      KCONST(116) = +1
      LIMIT(116) = 330
      KCONST(162) = +1
      LIMIT(162) = 330
      KCONST(166) = +1
      LIMIT(166) = 525
      KCONST(168) = +1
      LIMIT(168) = 390
      KCONST(169) = +1
      LIMIT(169) = 315
      KCONST(183) = +1
      LIMIT(183) = 390
      KCONST(190) = +1
      LIMIT(190) = 375
C
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1251 PRELIMINARY
 1251 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(7) = +1
      LIMIT(7) = 315
      KCONST(112) = +1
      LIMIT(112) = 105
      KCONST(116) = +1
      LIMIT(116) = 330
      KCONST(162) = +1
      LIMIT(162) = 330
      KCONST(166) = +1
      LIMIT(166) = 525
      KCONST(168) = +1
      LIMIT(168) = 390
      KCONST(169) = +1
      LIMIT(169) = 315
      KCONST(183) = +1
      LIMIT(183) = 390
      KCONST(190) = +1
      LIMIT(190) = 375
C
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1252 PRELIMINARY
 1252 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(7) = +1
      LIMIT(7) = 315
      KCONST(112) = +1
      LIMIT(112) = 105
      KCONST(166) = +1
      LIMIT(166) = 525
      KCONST(169) = +1
      LIMIT(169) = 315
C
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
C
      RETURN
C
C1254 PRELIMINARY
 1254 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(7) = +1
      LIMIT(7) = 315
      KCONST(112) = +1
      LIMIT(112) = 105
      KCONST(166) = +1
      LIMIT(166) = 525
      KCONST(169) = +1
      LIMIT(169) = 315
C
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
C
      RETURN
C
C1256 PRELIMINARY
 1256 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(7) = +1
      LIMIT(7) = 315
      KCONST(112) = +1
      LIMIT(112) = 105
      KCONST(166) = +1
      LIMIT(166) = 525
      KCONST(169) = +1
      LIMIT(169) = 315
C
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
C
      RETURN
C
C1257 PRELIMINARY
 1257 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(7) = +1
      LIMIT(7) = 315
      KCONST(112) = +1
      LIMIT(112) = 105
      KCONST(166) = +1
      LIMIT(166) = 525
      KCONST(169) = +1
      LIMIT(169) = 315
C
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
C
      RETURN
C
 1258 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(156) = -1
      LIMIT(156) = 90
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(156) = .TRUE.
C
      RETURN
C
C1259 PRELIMINARY
 1259 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(156) = -1
      LIMIT(156) = 90
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
      LTHRES(156) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1260 PRELIMINARY
 1260 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(156) = -1
      LIMIT(156) = 90
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(156) = .TRUE.
C
      RETURN
C
C1261 PRELIMINARY
 1261 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(156) = -1
      LIMIT(156) = 90
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(156) = .TRUE.
C
      RETURN
C
C1262 PRELIMINARY
 1262 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(156) = -1
      LIMIT(156) = 90
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(156) = .TRUE.
C
      RETURN
C
C1263 PRELIMINARY
 1263 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 180
      KCONST(156) = -1
      LIMIT(156) = 90
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(156) = .TRUE.
C
      RETURN
C
 1265 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(7) = +1
      LIMIT(7) = 315
      KCONST(25) = +1
      LIMIT(25) = 345
      KCONST(43) = +1
      LIMIT(43) = 345
      KCONST(49) = +1
      LIMIT(49) = 390
      KCONST(79) = +1
      LIMIT(79) = 390
      KCONST(107) = +1
      LIMIT(107) = 360
      KCONST(116) = +1
      LIMIT(116) = 330
      KCONST(132) = +1
      LIMIT(132) = 360
      KCONST(156) = +1
      LIMIT(156) = 30
      KCONST(162) = +1
      LIMIT(162) = 330
      KCONST(166) = +1
      LIMIT(166) = 525
      KCONST(168) = +1
      LIMIT(168) = 390
      KCONST(183) = +1
      LIMIT(183) = 390
      KCONST(190) = +1
      LIMIT(190) = 375
C
      LTHRES(23) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(178) = .TRUE.
      LTHRES(180) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(184) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1266 PRELIMINARY
 1266 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(7) = +1
      LIMIT(7) = 315
      KCONST(25) = +1
      LIMIT(25) = 345
      KCONST(43) = +1
      LIMIT(43) = 345
      KCONST(49) = +1
      LIMIT(49) = 390
      KCONST(79) = +1
      LIMIT(79) = 390
      KCONST(107) = +1
      LIMIT(107) = 360
      KCONST(116) = +1
      LIMIT(116) = 330
      KCONST(132) = +1
      LIMIT(132) = 360
      KCONST(156) = +1
      LIMIT(156) = 30
      KCONST(162) = +1
      LIMIT(162) = 330
      KCONST(166) = +1
      LIMIT(166) = 525
      KCONST(168) = +1
      LIMIT(168) = 390
      KCONST(183) = +1
      LIMIT(183) = 390
      KCONST(190) = +1
      LIMIT(190) = 375
C
      LTHRES(23) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(178) = .TRUE.
      LTHRES(180) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(184) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1267 PRELIMINARY
 1267 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(7) = +1
      LIMIT(7) = 315
      KCONST(25) = +1
      LIMIT(25) = 345
      KCONST(43) = +1
      LIMIT(43) = 345
      KCONST(49) = +1
      LIMIT(49) = 390
      KCONST(79) = +1
      LIMIT(79) = 390
      KCONST(107) = +1
      LIMIT(107) = 360
      KCONST(132) = +1
      LIMIT(132) = 360
      KCONST(156) = +1
      LIMIT(156) = 30
      KCONST(166) = +1
      LIMIT(166) = 525
C
      LTHRES(23) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(178) = .TRUE.
      LTHRES(180) = .TRUE.
      LTHRES(184) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1268 PRELIMINARY
 1268 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(7) = +1
      LIMIT(7) = 315
      KCONST(25) = +1
      LIMIT(25) = 345
      KCONST(43) = +1
      LIMIT(43) = 345
      KCONST(49) = +1
      LIMIT(49) = 390
      KCONST(79) = +1
      LIMIT(79) = 390
      KCONST(107) = +1
      LIMIT(107) = 360
      KCONST(116) = +1
      LIMIT(116) = 330
      KCONST(132) = +1
      LIMIT(132) = 360
      KCONST(156) = +1
      LIMIT(156) = 30
      KCONST(166) = +1
      LIMIT(166) = 525
C
      LTHRES(23) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(112) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(172) = .TRUE.
      LTHRES(178) = .TRUE.
      LTHRES(180) = .TRUE.
      LTHRES(184) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
 1269 LCHANN(79) = .TRUE.
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
        DO 12691 I = 97,190
12691   LTHRES(I) = 0
C
      RETURN
C
C1270 PRELIMINARY
 1270 LCHANN(79) = .TRUE.
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
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1271 PRELIMINARY
 1271 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
C1272 PRELIMINARY
 1272 LCHANN(79) = .TRUE.
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
      LTHRES(116) = .TRUE.
C
      RETURN
C
 1273 LCHANN(79) = .TRUE.
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
      LTHRES(116) = .TRUE.
C
      RETURN
C
C1274 PRELIMINARY
 1274 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
C1275 PRELIMINARY
 1275 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
 1276 LCHANN(79) = .TRUE.
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
        DO 12761 I = 97,190
12761   LTHRES(I) = .TRUE.
C
      RETURN
C
C1277 PRELIMINARY
 1277 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
C1280 PRELIMINARY
 1280 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
C1281 PRELIMINARY
 1281 LCHANN(79) = .TRUE.
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
 1282 LCHANN(79) = .TRUE.
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
C
      RETURN
C
C1283 PRELIMINARY
 1283 LCHANN(79) = .TRUE.
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
      LTHRES(116) = .TRUE.
C
      RETURN
C
 1286 LCHANN(79) = .TRUE.
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
      LTHRES(49) = .TRUE.
      LTHRES(116) = .TRUE.
C
      RETURN
C
 1287 LCHANN(79) = .TRUE.
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
      LTHRES(79) = .TRUE.
C
      RETURN
C
C1289 PRELIMINARY
 1289 LCHANN(79) = .TRUE.
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
      LTHRES(79) = .TRUE.
C
      RETURN
C
C1290 PRELIMINARY
 1290 LCHANN(79) = .TRUE.
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
      LTHRES(79) = .TRUE.
C
      RETURN
C
C1291 PRELIMINARY
 1291 LCHANN(79) = .TRUE.
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
      LTHRES(79) = .TRUE.
C
      RETURN
C
 1292 LCHANN(79) = .TRUE.
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
      RETURN
C
C1293 PRELIMINARY
 1293 LCHANN(79) = .TRUE.
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
      RETURN
C
C1294 PRELIMINARY
 1294 LCHANN(79) = .TRUE.
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
      RETURN
C
C1295 PRELIMINARY
 1295 LCHANN(79) = .TRUE.
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
      RETURN
C
C1296 PRELIMINARY
 1296 LCHANN(79) = .TRUE.
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
      RETURN
C
C1297 PRELIMINARY
 1297 LCHANN(79) = .TRUE.
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
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1299 PRELIMINARY
 1299 LCHANN(79) = .TRUE.
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
      RETURN
C
C1300 PRELIMINARY
 1300 LCHANN(79) = .TRUE.
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
      RETURN
C
C1301 PRELIMINARY
 1301 LCHANN(79) = .TRUE.
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
      RETURN
C
 1302 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
C1303 PRELIMINARY
 1303 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
 1304 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(49) =  .TRUE.
C
      RETURN
C
C1310 PRELIMINARY
 1310 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(49) =  .TRUE.
C
      RETURN
C
C1311 PRELIMINARY
 1311 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(49) =  .TRUE.
C
      RETURN
C
C1312 PRELIMINARY
 1312 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(49) =  .TRUE.
C
      RETURN
C
C1313 PRELIMINARY
 1313 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(49) =  .TRUE.
C
      RETURN
C
C1314 PRELIMINARY
 1314 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(49) =  .TRUE.
C
      RETURN
C
C1315 PRELIMINARY
 1315 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(49) =  .TRUE.
C
      RETURN
C
 1317 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
C1318 PRELIMINARY
 1318 LCHANN(79) = .TRUE.
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
C
      RETURN
C
C1319 PRELIMINARY
 1319 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
C1320 PRELIMINARY
 1320 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
 1321 LCHANN(79) = .TRUE.
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
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(186) = .TRUE.
C
      RETURN
C
C1326 PRELIMINARY
 1326 LCHANN(79) = .TRUE.
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
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(186) = .TRUE.
C
      RETURN
C
C1327 PRELIMINARY
 1327 LCHANN(79) = .TRUE.
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
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(186) = .TRUE.
C
      RETURN
C
C1328 PRELIMINARY
 1328 LCHANN(79) = .TRUE.
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
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(186) = .TRUE.
C
      RETURN
C
C1329 PRELIMINARY
 1329 LCHANN(79) = .TRUE.
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
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(186) = .TRUE.
C
      RETURN
C
C1330 PRELIMINARY
 1330 LCHANN(79) = .TRUE.
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
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(49) = .TRUE.
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(186) = .TRUE.
C
      RETURN
C
C1331 PRELIMINARY
 1331 LCHANN(79) = .TRUE.
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
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(186) = .TRUE.
C
      RETURN
C
C1335 PRELIMINARY
 1335 LCHANN(79) = .TRUE.
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
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(186) = .TRUE.
C
      RETURN
C
C1336 PRELIMINARY
 1336 LCHANN(79) = .TRUE.
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
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 225
      KCONST(183) = -1
      LIMIT(183) = 240
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(85) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(186) = .TRUE.
C
      RETURN
C
 1337 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
C1339 PRELIMINARY
 1339 LCHANN(79) = .TRUE.
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
      LTHRES(116) = .TRUE.
C
      RETURN
C
C1340 PRELIMINARY
 1340 LCHANN(79) = .TRUE.
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
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(116) = .TRUE.
      LTHRES(156) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1341 PRELIMINARY
 1341 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
 1344 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
C1345 PRELIMINARY
 1345 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
C1346 PRELIMINARY
 1346 LCHANN(79) = .TRUE.
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
      LTHRES(116) = .TRUE.
C
      RETURN
C
 1347 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
C1348 PRELIMINARY
 1348 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
C1353 PRELIMINARY
 1353 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
C1357 PRELIMINARY
 1357 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
C1358 PRELIMINARY
 1358 LCHANN(79) = .TRUE.
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
      LTHRES(116) = .TRUE.
C
      RETURN
C
 1359 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
C1360 PRELIMINARY
 1360 LCHANN(79) = .TRUE.
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
      LTHRES(116) = .TRUE.
C
      RETURN
C
C1361 PRELIMINARY
 1361 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
C1362 PRELIMINARY
 1362 LCHANN(79) = .TRUE.
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
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
 1364 LCHANN(79) = .TRUE.
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
      LTHRES(116) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
 1365 LCHANN(79) = .TRUE.
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
      LTHRES(116) = .TRUE.
C
      RETURN
C
 1366 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      RETURN
C
 1367 LCHANN(79) = .TRUE.
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
      LTHRES(116) = .TRUE.
C
      RETURN
C
 1368 LCHANN(79) = .TRUE.
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
      LTHRES(116) = .TRUE.
      LTHRES(171) = .TRUE.
C
      RETURN
C
 1369 LCHANN(79) = .TRUE.
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
      LIMIT(168) = 210
      KCONST(183) = -1
      LIMIT(183) = 195
      KCONST(190) = -1
      LIMIT(190) = 210
C
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
 1371 LCHANN(79) = .TRUE.
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
      LTHRES(116) = .TRUE.
C
      RETURN
C
C1372 PRELIMINARY
 1372 LCHANN(79) = .TRUE.
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
      LTHRES(116) = .TRUE.
C
      RETURN
C
C1373 PRELIMINARY
 1373 LCHANN(79) = .TRUE.
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
C
      RETURN
C
 1375 LCHANN(79) = .TRUE.
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
      KCONST(190) = -1
      LIMIT(190) = 225
C
      LTHRES(49) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1376 PRELIMINARY
 1376 LCHANN(79) = .TRUE.
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
C
      RETURN
C
C1377 PRELIMINARY
 1377 LCHANN(79) = .TRUE.
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
      LTHRES(49) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(162) = .TRUE.
      LTHRES(168) = .TRUE.
      LTHRES(183) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1378 PRELIMINARY
 1378 LCHANN(79) = .TRUE.
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
C
      RETURN
C
C1379 PRELIMINARY
 1379 LCHANN(79) = .TRUE.
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
C
      RETURN
C
C1380 PRELIMINARY
 1380 LCHANN(79) = .TRUE.
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
      LTHRES(116) = .TRUE.
C
      RETURN
C
 1381 LCHANN(79) = .TRUE.
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
      LTHRES(11) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(27) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(87) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(171) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
C1382 PRELIMINARY
 1382 LCHANN(79) = .TRUE.
      LCHANN(107) = .TRUE.
      LCHANN(156) = .TRUE.
      LCHANN(171) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 375
C
      LTHRES(11) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(27) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(87) = .TRUE.
      LTHRES(91) = .TRUE.
C
      RETURN
C
C1383 PRELIMINARY
 1383 LCHANN(79) = .TRUE.
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
      LTHRES(11) = .TRUE.
      LTHRES(23) = .TRUE.
      LTHRES(27) = .TRUE.
      LTHRES(53) = .TRUE.
      LTHRES(59) = .TRUE.
      LTHRES(63) = .TRUE.
      LTHRES(69) = .TRUE.
      LTHRES(72) = .TRUE.
      LTHRES(78) = .TRUE.
      LTHRES(84) = .TRUE.
      LTHRES(87) = .TRUE.
      LTHRES(91) = .TRUE.
      LTHRES(116) = .TRUE.
      LTHRES(190) = .TRUE.
C
      RETURN
C
 1384 LCHANN(79) = .TRUE.
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
      RETURN
C
C1385 PRELIMINARY
 1385 LCHANN(79) = .TRUE.
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
      RETURN
C
C1386 PRELIMINARY
 1386 LCHANN(79) = .TRUE.
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
      RETURN
C
C1393 PRELIMINARY
 1393 LCHANN(79) = .TRUE.
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
      RETURN
C
C1395 PRELIMINARY
 1395 LCHANN(79) = .TRUE.
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
      RETURN
C
C1397 PRELIMINARY
 1397 LCHANN(79) = .TRUE.
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
      RETURN
C
      END
