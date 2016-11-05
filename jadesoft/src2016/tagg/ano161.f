C   05/12/79 001241837  MEMBER NAME  ANO161   (S)           FORTRAN
      SUBROUTINE ANO161(IRUN)
C---  ANOMALIES OF RUNS 2386 - 2447
C---  6.0 GEV      NOVEMBER 1979
C
C     H.WRIEDT    27.07.79     02:20
C     LAST MODIFICATION     24.01.80     18:35
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
      GOTO (2386,2387,2388,2389,2390,2391,2392,2393,2394,2395,2396,13,
     *      2398,2399,2400,2401,2402,2403,2404,2405,2406,2407,2408,2409,
     *      2410,
     *      2411,2412,2413,2414,2415,2416,2417,2418,2419,13,2421,2422,
     *      2423,2424,2425,2426,2427,2428,2429,2430,2431,2432,2433,
     *      2434,2435,2436,2437,2438,2439,2440,2441,2442,2443,2444,
     *      2445,2446,2447),IRUN
   13 RETURN
C
 2386 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 2387 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(52) = +1
      LIMIT(52) = 390
      KCONST(60) = +1
      LIMIT(60) = 330
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(156) = +1
      LIMIT(156) = 210
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 2388 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 2389 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(69) = +1
      LIMIT(69) = 255
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 2390 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(183) = -1
      LIMIT(183) = 105
C
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2391 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2392 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2393 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(45) = +1
      LIMIT(45) = 405
      KCONST(55) = +1
      LIMIT(55) = 285
      KCONST(64) = +1
      LIMIT(64) = 450
      KCONST(67) = +1
      LIMIT(67) = 180
      KCONST(160) = +1
      LIMIT(160) = 300
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(178) = -1
      LIMIT(178) = 150
      KCONST(184) = -1
      LIMIT(184) = 90
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 2394 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(53) = +1
      LIMIT(53) = 315
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2395 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 2396 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2398 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2399 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2400 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2401 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(25) = +1
      LIMIT(25) = 315
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2402 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2403 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(110) = +1
      LIMIT(110) = 345
      KCONST(119) = +1
      LIMIT(119) = 360
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2404 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2405 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2406 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2407 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2408 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2409 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2410 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2411 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 90
      KCONST(178) = -1
      LIMIT(178) = 150
      KCONST(184) = -1
      LIMIT(184) = 105
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 2412 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2413 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
 2414 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2415 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2416 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(77) = +1
      LIMIT(77) = 405
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2417 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2418 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2419 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2421 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2422 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2423 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 90
      KCONST(178) = -1
      LIMIT(178) = 150
      KCONST(179) = +1
      LIMIT(179) = 315
      KCONST(184) = -1
      LIMIT(184) = 105
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 2424 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2425 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 90
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 2426 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2427 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(43) = +1
      LIMIT(43) = 315
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2428 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(7) = +1
      LIMIT(7) = 210
      KCONST(14) = +1
      LIMIT(14) = 390
      KCONST(111) = +1
      LIMIT(111) = 315
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2429 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2430 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(148) = +1
      LIMIT(148) = 285
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2431 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(40) = +1
      LIMIT(40) = 345
      KCONST(50) = +1
      LIMIT(50) = 390
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2432 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(41) = +1
      LIMIT(41) = 390
      KCONST(42) = +1
      LIMIT(42) = 345
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2433 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2434 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(13) = +1
      LIMIT(13) = 390
      KCONST(102) = +1
      LIMIT(102) = 300
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(162) = -1
      LIMIT(162) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 105
      KCONST(183) = -1
      LIMIT(183) = 105
      KCONST(184) = -1
      LIMIT(184) = 135
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 2435 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2436 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(31) = +1
      LIMIT(31) = 330
      KCONST(41) = +1
      LIMIT(41) = 360
      KCONST(127) = +1
      LIMIT(127) = 360
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2437 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(162) = -1
      LIMIT(162) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 105
      KCONST(178) = -1
      LIMIT(178) = 150
      KCONST(184) = -1
      LIMIT(184) = 105
      KCONST(190) = -1
      LIMIT(190) = 135
C
      RETURN
C
 2438 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2439 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(55) = .TRUE.
      LTHRES(57) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2440 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(73) = +1
      LIMIT(73) = 405
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = +1
      LIMIT(168) = 195
      KCONST(169) = +1
      LIMIT(169) = 210
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2441 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(69) = +1
      LIMIT(69) = 255
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2442 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2443 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2444 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2445 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2446 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(162) = -1
      LIMIT(162) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 105
      KCONST(178) = -1
      LIMIT(178) = 150
      KCONST(184) = -1
      LIMIT(184) = 105
      KCONST(190) = -1
      LIMIT(190) = 135
C
      RETURN
C
 2447 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(26) = +1
      LIMIT(26) = 345
      KCONST(112) = +1
      LIMIT(112) = 75
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(190) = -1
      LIMIT(190) = 135
C
      RETURN
C
      END
