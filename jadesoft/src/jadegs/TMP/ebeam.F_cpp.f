C   29/07/79 803301133  MEMBER NAME  EBEAM    (JADEGS)      FORTRAN
      FUNCTION EBEAM(HRUN)
      IMPLICIT INTEGER*2 (H)
C                                  LAST CHANGE   02.09.86
C            THE OLD VERSON IS AVAILABLE AS EBEAM0
C
C----------------------------------------------------------------------
C             MACRO CDATA .... BOS COMMON.
C
C             THIS MACRO ONLY DEFINES THE IDATA/HDATA/ADATA NAMES.
C             THE ACTUAL SIZE OF /BCS/ IS FIXED ON MACRO CBCSMX
C             OR BY OTHER MEANS. A DEFAULT SIZE OF 40000 IS GIVEN HERE.
C
C----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(40000)
      DIMENSION HDATA(80000),ADATA(40000),IPNT(50)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1)),(IPNT(1),IDATA(55))
      EQUIVALENCE (NWORD,IPNT(50))
C
C------------------------ END OF MACRO CDATA --------------------------
      DIMENSION HLIST(2,315),HLIST1(2,75),HLIST2(2,77),HLIST3(2,74)
      DIMENSION HLIST4(2,60),HLIST5(2,29)
      EQUIVALENCE(HLIST(1,  1),HLIST1(1,1))
      EQUIVALENCE(HLIST(1, 76),HLIST2(1,1))
      EQUIVALENCE(HLIST(1,153),HLIST3(1,1))
      EQUIVALENCE(HLIST(1,227),HLIST4(1,1))
      EQUIVALENCE(HLIST(1,287),HLIST5(1,1))
      DATA HLIST1/
     1  100,13600,  564,13860,  672,15783, 1083,11000, 1084,13860,
     1 1122,11000, 1164,15000, 1497,15250, 1560,15260, 1577,15270,
     1 1609,15280, 1627,15290, 1638,15300, 1649,15310, 1662,15320,
     1 1687,15330, 1714,15340, 1725,15350, 1730,15360, 1740,15370,
     1 1750,15380, 1756,15390, 1763,15400, 1770,15410, 1779,15420,
     1 1784,15430, 1796,15440, 1802,15450, 1810,15460, 1866,15470,
     1 1869,15500, 1872,15470, 1876,15480, 1882,15490, 1889,15500,
     1 1893,15510, 1898,15520, 1905,15530, 1909,15540, 1912,15550,
     1 1920,15560, 1929,15570, 1935,15580, 1941,15590, 1946,15600,
     1 1953,15610, 1959,15620, 1966,15630, 1970,15640, 1973,15650,
     1 1978,15660, 1987,15670, 2009,15680, 2018,15690, 2026,15700,
     1 2033,15710, 2042,15720, 2052,15730, 2056,15000, 2063,15010,
     1 2070,15020, 2077,15030, 2083,15040, 2092,15050, 2097,15060,
     1 2112,15070, 2117,15080, 2122,15090, 2126,15100, 2130,15110,
     1 2134,15120, 2146,15130, 2150,15140, 2153,15150, 2155,15160/
      DATA HLIST2/
     1 2160,15170, 2164,15180, 2168,15190, 2177,15200, 2183,15210,
     1 2188,15220, 2191,15230, 2195,15240, 2202,14950, 2208,14960,
     1 2217,14970, 2220,14980, 2223,14990, 2295, 6000, 2521,16000,
     1 2748,16500, 2749,17000, 2750,17500, 2751,16500, 2756,17400,
     1 2758,16500, 2803,17000, 2828,17500, 3031,17900, 3136,17500,
     1 3143,17510, 3151,17520, 3160,17530, 3172,17540, 3185,17550,
     1 3200,17560, 3215,17570, 3227,17580, 3238,17590, 3247,17600,
     1 3259,17610, 3273,17620, 3291,17630, 3325,17600, 3343,17610,
     1 3362,17620, 3377,17630, 3390,17640, 3399,17650, 3411,17660,
     1 3423,17670, 3430,17688, 3433,17670,
     1             3440,17680, 3457,17690, 3472,17700, 3492,17710,
     1 3504,14950, 3519,14960, 3531,14970, 3561,18300, 3590,14950,
     1 3608,18300, 3615,17720, 3630,17730, 3637,17733, 3643,17740,
     1 3653,17742, 3656,17750, 3677,17760, 3699,17770, 3715,17780,
     1 3728,18300, 3847,18251, 3849,18200, 3859,18300, 3861,18200,
     1 3938,17520, 3945,17510, 3952,17500, 3972,17490, 3986,17480/
      DATA HLIST3/
     1 3996,17470, 4009,17460, 4018,17450, 4025,17440, 4032,17430,
     1 4045,17420, 4061,17410, 4073,17400, 4081,17390, 4088,17380,
     1 4095,17370, 4100,17360, 4108,17350, 4117,17340, 4125,17330,
     1 4134,17320, 4147,17310, 4165,17300, 4180,17290, 4188,17280,
     1 4193,17290, 4200,17280, 4204,17270, 4212,17260, 4220,17250,
     1 4232,17240, 4239,17230, 4248,17220, 4259,17210, 4266,17200,
     1 4272,17190, 4281,17180, 4286,17000, 4294,17010, 4303,17020,
     1 4311,17030, 4319,17040, 4326,17050, 4334,17060, 4342,17070,
     1 4347,17080, 4353,17090, 4357,17100, 4360,17110, 4366,17120,
     1 4375,17130, 4382,17140, 4390,17150, 4398,17160, 4406,17170,
     1 4413,17794, 4429,17818, 4438,17828, 4448,17838, 4456,17848,
     1             4466,17858, 4479,17868, 4500,17878, 4506,16500,
     1 4531,16510, 4538,16520, 4544,16530, 4551,16540, 4562,16550,
     1 4569,16560, 4577,16570, 4586,16580, 4597,16590, 4605,16600,
     1 4617,16610, 4626,16620, 4633,16630, 4642,16640, 4655,16650/
      DATA HLIST4/
     1 4660,16660, 4667,16670, 4675,16680, 4681,16690, 4688,16700,
     1 4696,16710, 4702,16720, 4708,16730, 4717,16740, 4726,16750,
     1 4732,16760, 4740,16770, 4746,16780, 4751,16790, 4756,16800,
     1 4763,16810, 4774,16820, 4781,16830, 4794,16840, 4804,16850,
     1 4810,16860, 4817,16870, 4825,16880, 4831,16890, 4837,16900,
     1 4842,16910, 4861,16920, 4993,17867, 4997,17860, 4999,17874,
     1 5003,17860, 5041,17870, 5057,17880, 5076,17890, 5090,17900,
     1 5103,17910, 5116,17920, 5127,17930, 5148,17940, 5163,17950,
     1 5182,17960, 5202,17970, 5218,17980, 5234,17990, 5248,18000,
     1 5267,18010, 5293,18020, 5309,18030, 5330,18040, 5346,18050,
     1 5354,18060, 5364,18070, 5373,18080, 5380,18090, 5391,18100,
     1 5398,18110, 5404,18120, 5415,18130, 5420,18140, 5427,18150/
      DATA HLIST5/
     1 5437,18160, 5446,18170, 5456,18180, 5469,18191, 5475,18190,
     1 5477,18200, 5492,18210, 5500,18220, 5508,18230, 5516,18240,
     1 5527,18250, 5537,18260, 5542,18270, 5548,18280, 5553,18291,
     1 5561,18300, 5568,18310, 5582,18320, 5584,18321, 5590,18320,
     1 5604,18330, 5615,18340, 5637,18350, 5653,18360, 5659,18350,
     1 5660,18360, 5663,16900, 5666,18360, 5700,18360/
C---
      DATA ICALL/0/
C
      IF(HRUN.GT.5667) GO TO 9797
      IF(HRUN.GT.100) GO TO 1
9797  IHEAD = IDATA(IBLN('HEAD'))
      IF(IHEAD.LE.0) GO TO 2
      EBEAM= HDATA(IHEAD*2 + 29)
      IF(HRUN.GT.11037.AND.HRUN.LT.11053) EBEAM = 17243.
      IF(HRUN.EQ.11104.OR.HRUN.EQ.10666) EBEAM = 17300.
      IF(HRUN.EQ.11790.OR.HRUN.EQ.11791) EBEAM = 17300.
      IF(HRUN.EQ.12804.OR.HRUN.EQ.12805) EBEAM = 19120.
      IF(HRUN.GE.13707.AND.HRUN.LE.13713) EBEAM = 20725.
      IF(HRUN.EQ.13714) EBEAM = 20740.
      IF(HRUN.EQ.13826) EBEAM = 20795.
      IF(HRUN.EQ.16342) EBEAM = 23240.
      IF(HRUN.EQ.22357) EBEAM = 21800.
      IF(HRUN.EQ.26435) EBEAM = 17500.
C
C                                     PRINTOUT(WARNING) LOW BEAM ENERGY
C
         IF(EBEAM.LT.5000.) WRITE(6,5404) HRUN,EBEAM
5404  FORMAT(' @ EBEAM WARNING @@@@@@@: RUN ',I6,' HAS BEAM ENERGY = '
     $,E12.4,' MEV ')
      IF(HRUN.GE.18283.AND.HRUN.LE.18287) WRITE(6,5503) HRUN
      IF(HRUN.GE.17691.AND.HRUN.LE.17695) WRITE(6,5503) HRUN
      IF(HRUN.GE.11796.AND.HRUN.LE.11798) WRITE(6,5503) HRUN
      IF(HRUN.GE.15869.AND.HRUN.LE.15870) WRITE(6,5503) HRUN
      IF(HRUN.GE.8630.AND.HRUN.LE.8631) WRITE(6,5503) HRUN
      IF(HRUN.EQ.25688) WRITE(6,5503) HRUN
      IF(HRUN.EQ.15870) WRITE(6,5503) HRUN
5503  FORMAT(' EBEAM:    COSMIC DATA !!!   RUN ',I6)
      IF(HRUN.EQ. 6185) WRITE(6,5504) HRUN
5504  FORMAT(' EBEAM:    TEST DATA !!!   RUN ',I6)
C
      RETURN
2     WRITE(6,3)
3     FORMAT('  ****** ERROR IN EBEAM ****  NO HEAD BANK ')
      EBEAM = 0.
      RETURN
1     NRUN = HRUN
      NR1 = 1
      NR2 = 88
      IF(NRUN.LT.2294) GO TO 44
      NR1 = 89
      NR2 = 157
      IF(NRUN.LT.4045) GO TO 44
      NR1 = 158
      NR2 = 223
      IF(NRUN.LT.4633) GO TO 44
      NR1 = 224
      NR2 = 273
      IF(NRUN.LT.5309) GO TO 44
      NR1 = 274
      NR2 = 315
44    MARK = 0
      DO 10  IR = NR1,NR2
      IF(NRUN.GE.HLIST(1,IR+1)) GO TO 10
      EBEAM = HLIST(2,IR)
      MARK = 1
      GO TO 20
10    CONTINUE
20    IF(MARK.NE.0) GO TO 30
      WRITE(6,21) NRUN
21    FORMAT(' * * * WARNING * * * ERROR IN EBEAM AT RUN ',I6,' PLEASE T
     $ELL J.OLSSON')
      EBEAM= 0.
      RETURN
30    CONTINUE
999   IF(HRUN.LT.3325.OR.HRUN.GT.3937) RETURN
      IF(HRUN.GT.3325.AND.HRUN.LT.3504) EBEAM= EBEAM+ 20.
      IF(HRUN.GT.3560.AND.HRUN.LT.3590) EBEAM= EBEAM+ 35.
      IF(HRUN.GT.3607.AND.HRUN.LT.3615) EBEAM= EBEAM+ 30.
      IF(HRUN.GT.3614.AND.HRUN.LT.3938) EBEAM= EBEAM+ 35.
      ICALL = ICALL + 1
      IF(ICALL.EQ.1) WRITE(6,2141)
2141  FORMAT(' EBEAM:   BEAM ENERGY NOW CORRECTED FOR FREQUENCY SHIFT IN
     $ RUN PERIOD 3325 - 3937')
      RETURN
      END
