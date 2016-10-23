C   11/01/87 803311522  MEMBER NAME  ZE4VUN   (S)           FORTRAN77
      SUBROUTINE ZE4VUN( NST, N, IMODE )
C-----------------------------------------------------------
C LAST MOD 31-03-88 M ZIMMER
C
C NEW ENTRY ZE4PUN MADE TO WORK
C NEW ENTRY INCLUDED ZE4PUN
C NEW FORMAT OF BANK INCLUDED
C UNPACK MOMENTA FROM ZE4V INTO VECSUB COMMON
C IMODE = 0 CHARGED AND GAMMA
C       = 1
C       = 2
C       = 3 ALL CLUSTERS
C       = 4 CHARGED ONLY ( ALL MASSES PI )
C THE PARTICLES ARE STORED IN
C NST+1..NST+N, WHERE N IS SET ON EXIT
C    THE INDIVIDUAL POSITIONS ARE:
C   P( 1..7,*)   PX,PY,PZ,E,M,P,CHARGE
C  HP(15   ,*)   NOT USED
C  HP(16   ,*)   PARTICLE TYPE ( JADE MC-CONVENTION )
C  HP(17   ,*)   PART # IN ZE4V BANK
C  HP(18   ,*)   PATR NUMBER OF PHOT PARTNER
C  HP(19   ,*)   PALL NUMBER
C  HP(20   ,*)   ELECTRON ORIGIN
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / BCS / IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1),RW(1),IW(1))
C
      COMMON P(10,400)
      DIMENSION IP(10,100),HP(20,1)
      EQUIVALENCE (P(1,1),IP(1,1),HP(1,1))
C
      DIMENSION LADD(-1:2)
C
C                                           PARTICLE TYPES
C         UNKNOWN, GAMMA,   E,    MU,     PI,    K,    P/N,  LAM
      REAL*4 XMAS(8) /
     *     .1396,   0., .511E-3, .1067, .1396, .4937, .939, 1.1156 /
C
      DATA  IFIRST / 0 /
C
      IF( IFIRST .EQ. 0 ) THEN
        WRITE(6,*) '+++  ZE4VUN CALLED VERSION FROM 22/02/87  +++'
        IFIRST = 1000
      ENDIF
C
C
      N = 0
      NPZE4V = IW(IBLN('ZE4V'))
      IF( NPZE4V.LE.0 ) GO TO 7000
C                                      PTF-CODE
C                                      -1=LT/0=LTNE/1=LTCH/2=LTRE
      LADD(-1) = HW( NPZE4V*2 + 5 )
      LADD(0) = HW( NPZE4V*2 + 9 ) + LADD(-1)
      LADD(1) = HW( NPZE4V*2 + 7 ) + LADD(-1)
      LADD(2) = HW( NPZE4V*2 + 11) + LADD(-1)
      NT = HW(NPZE4V*2 + 6 )
      NP = NPZE4V + HW( NPZE4V*2 + 1 )
      DO 6200 J=1,NT
            INFO = HW( NP*2 + 18 )
            CHARGE = RW( NP + 7)
            IF( CHARGE.EQ.0. .AND. IMODE.EQ.4 ) GO TO 6100
            IF( CHARGE.NE.0. .AND. IMODE.EQ.3 ) GO TO 6100
            IF( CHARGE.NE.0. .AND. IMODE.EQ.1 ) GO TO 6100
C                                      SCIP RECONSTRUCTED PARTICLES
            IF( HW( NP*2 + 18 ) .EQ. 2 ) GO TO 6100
               N = N + 1
               CALL UCOPY( RW(NP+1), P(1,NST+N), 7 )
               CALL SMUL( NST+N, NST+N, P(6,NST+N) )
*** PMF 09/12/99               ITYP = MOD(HW(NP*2+8),100)
* Shuffle first MOD argument into an integer*4 variable.
               ihelp=HW(NP*2+8)
               ITYP = MOD(ihelp,100)
*** PMF(end)
               IF( IMODE .EQ. 4 .AND. ITYP .GT. 1 ) ITYP = 4
               P(5,NST+N) = XMAS(ITYP+1)
               P(4,NST+N) = SQRT( P(6,NST+N)**2 + P(5,NST+N)**2 )
               HP(15,NST+N) = 0
               HP(16,NST+N) = HW(NP*2 + 8)
               HP(17,NST+N) = J
               HP(18,NST+N) = HW(NP*2 + 7)
               HP(19,NST+N) = HW(NP*2 +15)
               HP(20,NST+N) = HW(NP*2 +16)
 6100       CONTINUE
            NP = NP + LADD( HW( NP*2 + 18 ))
 6200    CONTINUE
 7000 CONTINUE
      RETURN
C
C
C
      ENTRY ZE4VAX( NAST, NA )
C-----------------------------------------------------------
C UNPACK AXES FROM ZE4V INTO VECSUB COMMON
C STORED:  NAST+1      SPHERICITY AXIS AND VALUE
C          NAST+2      2ND AXIS AND VALUE
C          NAST+3      3RD AXIS AND VALUE
C          NAST+4      THRUST AXIS AND VALUE
C          NAST+5      AKOPLANARITY AXIS AND VALUE
C NA = 5 = # OF AXES FILLED ON NORMAL RETURN
C-----------------------------------------------------------
C
C
      NA = 0
      NPZE4V = IW(IBLN('ZE4V'))
      IF( NPZE4V.LE.0 ) GO TO 8000
         CALL UCOPY( RW(NPZE4V+ 9), P(1,NAST+1), 4 )
         CALL UCOPY( RW(NPZE4V+13), P(1,NAST+2), 4 )
         CALL UCOPY( RW(NPZE4V+17), P(1,NAST+3), 4 )
         CALL UCOPY( RW(NPZE4V+21), P(1,NAST+4), 4 )
         CALL UCOPY( RW(NPZE4V+25), P(1,NAST+5), 4 )
         NA = 5
 8000 CONTINUE
      RETURN
      ENTRY ZE4PUN( NPST, NPART )
C-----------------------------------------------------------
C UNPACK PARTONS FROM ZE4V INTO VECSUB COMMON
C STORED:  NPST+1      1-ST PARTON 4-VECTOR
C             :
C          NPST+N      N-TH PARTON 4-VECTOR
C
C NPART = NUMBER OF PARTONS
C-----------------------------------------------------------
C
C
      NPART = 0
      NPZE4V = IW(IBLN('ZE4V'))
      IF( NPZE4V.GT.0 ) THEN

        NVX = HW(NPZE4V*2 + 3 )
C                                      LOOK FOR PARTONS
        IF( 31+3*NVX .LT. HW(NPZE4V*2+1) ) THEN
          NPART = HW((NPZE4V+31+3*NVX)*2+1)
          DO 1101 I = 1, NPART
            CALL UCOPY( RW(NPZE4V+31+3*NVX+4*(I-1)+2), P(1,NPST+I), 4 )
            CALL LENGTH( NPST+I, P(6,NPST+I) )
            P(5,NPST+I) = SQRT(AMAX1(0.,P(4,NPST+I)**2-P(6,NPST+I)**2))
            P(7,NPST+I) = 0.
 1101     CONTINUE
        ENDIF
      ENDIF
      RETURN
      END
