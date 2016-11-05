C   30/11/79 001241835  MEMBER NAME  ANO160   (S)           FORTRAN
      SUBROUTINE ANO160(IRUN)
C---  ANOMALIES OF RUNS 2306 - 2385
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
      GOTO (2306,2307,2308,2309,2310,2311,2312,13,13,13,13,2317,2318,
     *      2319,2320,2321,2322,2323,2324,2325,13,13,2328,2329,2330,
     *      2331,2332,2333,2334,2335,2336,2337,13,2339,2340,2341,2342,
     *      2343,2344,2345,2346,2347,2348,2349,2350,2351,2352,2353,2354,
     *      2355,2356,13,2358,2359,2360,2361,2362,2363,2364,2365,
     *      2366,2367,2368,2369,2370,2371,2372,2373,2374,2375,2376,
     *      2377,2378,2379,2380,2381,2382,2383,2384,2385),IRUN
   13 RETURN
C
 2306 LCHANN(107) = .TRUE.
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
C2307 PRELIMINARY
 2307 LCHANN(107) = .TRUE.
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
C2308 PRELIMINARY
 2308 LCHANN(107) = .TRUE.
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
C2309 PRELIMINARY
 2309 LCHANN(107) = .TRUE.
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
C2310 PRELIMINARY
 2310 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
C2311 PRELIMINARY
 2311 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(68) = +1
      LIMIT(68) = 360
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
C2312 PRELIMINARY
 2312 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(49) = .TRUE.
C
      RETURN
C
C2317 PRELIMINARY
 2317 LCHANN(107) = .TRUE.
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
C2318 PRELIMINARY
 2318 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(16) = +1
      LIMIT(16) = 390
      KCONST(111) = +1
      LIMIT(111) = 315
      KCONST(112) = +1
      LIMIT(112) = 75
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
C2319 PRELIMINARY
 2319 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2320 LCHANN(107) = .TRUE.
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
      LIMIT(168) = 120
      KCONST(183) = -1
      LIMIT(183) = 120
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 2321 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2322 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(59) = +1
      LIMIT(59) = 360
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(162) = -1
      LIMIT(162) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(183) = -1
      LIMIT(183) = 120
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 2323 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(101) = -1
      LIMIT(101) = 180
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2324 LCHANN(107) = .TRUE.
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
 2325 LCHANN(107) = .TRUE.
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
 2328 LCHANN(107) = .TRUE.
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
 2329 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 105
      KCONST(178) = -1
      LIMIT(178) = 150
      KCONST(184) = -1
      LIMIT(184) = 105
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 2330 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(151) = +1
      LIMIT(151) = 360
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 2331 LCHANN(107) = .TRUE.
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
 2332 LCHANN(107) = .TRUE.
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
 2333 LCHANN(107) = .TRUE.
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
 2334 LCHANN(107) = .TRUE.
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
 2335 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(35) = +1
      LIMIT(35) = 375
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
 2336 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(57) = +1
      LIMIT(57) = 375
      KCONST(67) = +1
      LIMIT(67) = 165
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(178) = -1
      LIMIT(178) = 150
      KCONST(184) = -1
      LIMIT(184) = 120
      KCONST(190) = -1
      LIMIT(190) = 150
C
      LTHRES(154) = .TRUE.
C
      RETURN
C
 2337 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(7) = -1
      LIMIT(7) = 105
      KCONST(49) = -1
      LIMIT(49) = 225
      KCONST(55) = -1
      LIMIT(55) = 165
      KCONST(57) = -1
      LIMIT(57) = 300
      KCONST(67) = -1
      LIMIT(67) = 75
      KCONST(155) = -1
      LIMIT(155) = 360
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(162) = .TRUE.
C
      RETURN
C
 2339 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2340 LCHANN(107) = .TRUE.
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
 2341 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(101) = -1
      LIMIT(101) = 210
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
 2342 LCHANN(107) = .TRUE.
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
 2343 LCHANN(107) = .TRUE.
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
 2344 LCHANN(107) = .TRUE.
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
 2345 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2346 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(7) = +1
      LIMIT(7) = 225
      KCONST(49) = +1
      LIMIT(49) = 315
      KCONST(55) = +1
      LIMIT(55) = 285
      KCONST(57) = +1
      LIMIT(57) = 360
      KCONST(60) = +1
      LIMIT(60) = 330
      KCONST(67) = +1
      LIMIT(67) = 165
      KCONST(154) = -1
      LIMIT(154) = 90
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(178) = -1
      LIMIT(178) = 150
      KCONST(184) = -1
      LIMIT(184) = 120
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 2347 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(27) = +1
      LIMIT(27) = 270
      KCONST(35) = +1
      LIMIT(35) = 375
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2348 LCHANN(107) = .TRUE.
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
 2349 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(178) = -1
      LIMIT(178) = 150
      KCONST(188) = +1
      LIMIT(188) = 195
C
      LTHRES(7) = .TRUE.
      LTHRES(25) = .TRUE.
      LTHRES(43) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(57) = .TRUE.
      LTHRES(60) = .TRUE.
      LTHRES(67) = .TRUE.
      LTHRES(172) = .TRUE.
C
      RETURN
C
 2350 LCHANN(107) = .TRUE.
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
 2351 LCHANN(107) = .TRUE.
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
 2352 LCHANN(107) = .TRUE.
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
 2353 LCHANN(107) = .TRUE.
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
 2354 LCHANN(107) = .TRUE.
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
 2355 LCHANN(107) = .TRUE.
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
 2356 LCHANN(107) = .TRUE.
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
 2358 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(22) = +1
      LIMIT(22) = 390
      KCONST(23) = +1
      LIMIT(23) = 270
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(118) = +1
      LIMIT(118) = 270
      KCONST(119) = +1
      LIMIT(119) = 375
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2359 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(71) = +1
      LIMIT(71) = 360
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2360 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(82) = +1
      LIMIT(82) = 405
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2361 LCHANN(107) = .TRUE.
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
 2362 LCHANN(107) = .TRUE.
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
 2363 LCHANN(107) = .TRUE.
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
 2364 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2365 LCHANN(107) = .TRUE.
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
 2366 LCHANN(107) = .TRUE.
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
 2367 LCHANN(107) = .TRUE.
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
 2368 LCHANN(107) = .TRUE.
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
 2369 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(28) = +1
      LIMIT(28) = 345
      KCONST(124) = +1
      LIMIT(124) = 300
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2370 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(71) = +1
      LIMIT(71) = 360
      KCONST(80) = +1
      LIMIT(80) = 420
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(176) = +1
      LIMIT(176) = 255
      KCONST(178) = -1
      LIMIT(178) = 150
      KCONST(184) = -1
      LIMIT(184) = 105
      KCONST(190) = -1
      LIMIT(190) = 135
C
      RETURN
C
 2371 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      RETURN
C
 2372 LCHANN(107) = .TRUE.
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
 2373 LCHANN(107) = .TRUE.
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
 2374 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(71) = +1
      LIMIT(71) = 360
      KCONST(101) = -1
      LIMIT(101) = 210
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2375 LCHANN(107) = .TRUE.
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
      LIMIT(168) = 120
      KCONST(190) = -1
      LIMIT(190) = 135
C
      RETURN
C
 2376 LCHANN(107) = .TRUE.
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
      LIMIT(168) = 90
      KCONST(183) = -1
      LIMIT(183) = 120
      KCONST(190) = -1
      LIMIT(190) = 150
C
      RETURN
C
 2377 LCHANN(107) = .TRUE.
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
 2378 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2379 LCHANN(107) = .TRUE.
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
 2380 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(81) = +1
      LIMIT(81) = 360
      KCONST(82) = +1
      LIMIT(82) = 390
      KCONST(116) = -1
      LIMIT(116) = 120
      KCONST(162) = -1
      LIMIT(162) = 120
      KCONST(166) = -1
      LIMIT(166) = 360
      KCONST(168) = -1
      LIMIT(168) = 120
      KCONST(177) = +1
      LIMIT(177) = 240
      KCONST(183) = -1
      LIMIT(183) = 120
      KCONST(190) = -1
      LIMIT(190) = 150
C
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2381 LCHANN(107) = .TRUE.
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
 2382 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(125) = +1
      LIMIT(125) = 375
      KCONST(166) = -1
      LIMIT(166) = 360
C
      LTHRES(7) = .TRUE.
      LTHRES(49) = .TRUE.
      LTHRES(67) = .TRUE.
C
      RETURN
C
 2383 LCHANN(107) = .TRUE.
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
 2384 LCHANN(107) = .TRUE.
      LCHANN(173) = .TRUE.
      LCHANN(181) = .TRUE.
C
      KCONST(116) = -1
      LIMIT(116) = 120
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
 2385 LCHANN(107) = .TRUE.
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
      END
