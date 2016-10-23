C   11/01/87 702191103  MEMBER NAME  ZE4VUN   (S)           FORTRAN77
      SUBROUTINE ZE4VUN( NST, N, IMODE )
C-----------------------------------------------------------
C MODIFIED 17-11-86 M.ZIMMER
C LAST MOD 19-02-87 G.ECKERLIN  (SCIP OF RECONSTR. PART.)
C NEW FORMAT OF BANK INCLUDED
C UNPACK MOMENTA FROM ZE4V INTO VECSUB COMMON
C IMODE = 0 UNCONNECTED LG-CLUSTERS AND PATR
C       = 1 UNCONNECTED LG-CLUSTERS
C       = 2 ALL CLUSTERS AND PATR
C       = 3 ALL CLUSTERS
C       = 4 CHARGED ONLY ( ALL MASSES PI )
C THE PARTICLES ARE STORED IN
C NST+1..NST+N, WHERE N IS SET ON EXIT
C    THE INDIVIDUAL POSITIONS ARE:
C   P( 1..7,*)   PX,PY,PZ,E,M,P,CHARGE
C  HP(15   ,*)   NOT USED
C  HP(16   ,*)   PARTICLE TYPE ( JADE MC-CONVENTION )
C  HP(17   ,*)   PATR NUMBER OF PHOT PARTNER
C  HP(18   ,*)   PART # IN ZE4V BANK
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
            NUMMRN = HW( NP*2 + 13 )
            NUMMEV = HW( NP*2 + 14 )
            IF ( INFO .LT. -1 .OR. INFO .GT. 2 ) THEN
              WRITE (6,*) '------->>>>> INFO  ',INFO,
     &        '    NUMBER OF PARTICLES  ',NT,
     &        '  RUN#  ',NUMMRN,'  EVT#  ',NUMMEV
              RETURN
            ENDIF
            IF( CHARGE.EQ.0. .AND. IMODE.EQ.4 ) GO TO 6100
            IF( CHARGE.NE.0. .AND. IMODE.EQ.3 ) GO TO 6100
            IF( CHARGE.NE.0. .AND. IMODE.EQ.1 ) GO TO 6100
C                                      SCIP RECONSTRUCTED PARTICLES
            IF( HW( NP*2 + 18 ) .EQ. 2 ) GO TO 6100
C                                      SCIP SECONDARIES
CCC         IF( HW( NP*2 + 9 ) .GT. 1 ) GO TO 6100
               N = N + 1
               CALL UCOPY( RW(NP+1), P(1,NST+N), 7 )
               CALL SMUL( NST+N, NST+N, P(6,NST+N) )
               ITYP = HW(NP*2+8)
               P(5,NST+N) = XMAS(ITYP+1)
               P(4,NST+N) = SQRT( P(6,NST+N)**2 + P(5,NST+N)**2 )
               P( 8,NST+N) = 0
               HP(16,NST+N) = HW(NP*2 + 8)
               HP(17,NST+N) = HW(NP*2 + 7)
               HP(18,NST+N) = J
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
C NA = 4 ON NORMAL RETURN
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
         NA = 4
 8000 CONTINUE
      RETURN
      END
