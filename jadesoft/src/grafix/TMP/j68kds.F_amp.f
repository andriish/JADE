C   27/08/86 705201509  MEMBER NAME  J68KDS   (S)           FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE J68KDS
C-----------------------------------------------------------------------
C
C    AUTHOR:   G. ECKERLIN    5/08/86 :  JET CHAMBER RAW DATA DISPLAY
C       MOD:   G. ECKERLIN   18/08/86 :  NOW WITH USER KOORDINATES
C       MOD:   G. ECKERLIN   20/08/86 :  WIRE RANGE SELECTION ADDED
C       MOD:   G. ECKERLIN   27/08/86 :  DISPLAY OR NORMPULS ADDED
C       MOD:   G. ECKERLIN    3/09/86 :  DISPLAY DETAILS IMPLEMENTED
C  LAST MOD:   E. ELSEN      20/05/87 :  REDUCE SIZE OF BCS FROM 80K
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cgraph.for"
C
      LOGICAL LINEAR, LSEP, LDOS, LCELL(96), LWIFND, LWIRE(16),
     +        LAMP, DSPDTM
C
      INTEGER CL2PRC, PRC2CL
C
      COMMON / BCS / IW(1)
      COMMON /CWORK1/ IAMP(300,2), IWR, IPED(2,16), NWIRNG(2), IT0
     +                ,ISTWIR(16), IENWIR(16), NP0WIR(16)
     +                ,ISPRE2(8), TPREV(8), ISPRE1(8)
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
      COMMON / CHEADR / HEAD(108)
      DIMENSION HW(1), RW(1)
      EQUIVALENCE (HW(1), IW(1), RW(1))
C
      DIMENSION NPCELL(96), NENDCL(96)
C
      DATA IPRINT / 0 /, IPRLIM / 0 /
      DATA AFAC / .7588 /, SCALE / 10. /
C
      RAW2LN(J) = SCALE * J * 64 / (64 - AFAC * J) + .5
C
      LN2RAW(R) = R/(SCALE + R*AFAC/64)+ .5
C
C-----------------  C O D E  -------------------------------------------

      INCELL = ACMD
CX    WRITE (6,9030) INCELL
 9030 FORMAT(' INCELL :',I5)
C                                   GET GLOBAL T0 FOR RUN
      IRUN = HEAD(18)
      IT0 = IT0GLB(IRUN)
CX    WRITE (6,9033) IT0
 9033 FORMAT(' T0 :',I7)
C                                   SET POINTER FOR J68K
      NPJ68K = IW(IBLN('J68K'))

C                                   IF J68K-BANK IS FILLED
      IF( NPJ68K.GT.0 ) THEN

        IF( IPRINT .LT. IPRLIM ) THEN
          CALL BPRS( 'J68K', 0 )
        ENDIF
        IPRINT = IPRINT + 1

C                                   PONITER FOR SCAN_SEC
        NPSCAN = HW(NPJ68K*2+7)
 9000   FORMAT(' NPSCAN :',I6)
CX      WRITE(6,9000) NPSCAN

        IF( NPSCAN.GT.0 ) THEN

C                                   RESET VARIABLES
          DO 200 I = 1, 96
            NPCELL(I) = 0
            NENDCL(I) = 0
            LCELL(I) = .FALSE.
  200     CONTINUE

          NPRCL = 0
          NPSCAN = NPSCAN + NPJ68K*2

C                                   NUMBER OF CRATES WITH SCAN_DATA
          NCRATE = HW(NPSCAN+1)
          NPSCAN = NPSCAN + 1
 9001     FORMAT(' NUMBER OF CRATES :',I6)
CX        WRITE(6,9001) NCRATE

C                                   FIND CRATE * CELL NUMBERS
          DO 2000 ICRATE=1,NCRATE
            L2 = HW(NPSCAN+1)
 9002       FORMAT(' WORDS FOR CRATE :',I6)
CX          WRITE(6,9002) L2
            IF( L2 .GT. 0 ) THEN

C                                   CRATE NUMBER
              IDL300 = HW(NPSCAN+2)
 9003         FORMAT(' CRATE NUMBER :',I6)
CX            WRITE(6,9003) IDL300
              NPSCAN = NPSCAN + 2
              LPT0 = 2
  700         IF( LPT0 .GE. L2 ) GO TO 900

C                                   NUMBER OF WORDS FOR PULS TRAIN
                L3 =  HW(NPSCAN+1)
                LPT0 = LPT0 + L3
                IF( L3.GT.5 ) THEN
                  IWIRE = HW(NPSCAN+2)/256

C                                   CELL NUMBER
                  ICELL = PRC2CL(IDL300)
                  ICELL = ICELL + IWIRE/16 + 1
 9004             FORMAT(' CELL NUMBER :',I6)
CX                WRITE(6,9004) ICELL
                  IF (.NOT.LCELL(ICELL)) THEN
                    LCELL(ICELL) = .TRUE.
                    NPCELL(ICELL) = NPSCAN
                    IF (NPRCL.GT.0) NENDCL(NPRCL) = NPSCAN
                    NPRCL = ICELL
                  ENDIF
                ENDIF
                NPSCAN =NPSCAN + L3
                GOTO 700
  900         CONTINUE

              IF (NPRCL.GT.0) NENDCL(NPRCL) = NPSCAN

            ENDIF
 2000     CONTINUE
C
C                                   REQUEST FOR CELL NUMBER
          IF (INCELL.LT.-96 .OR. INCELL.GT.96 ) INCELL = 0
          IF (INCELL.EQ.0 .OR. (.NOT.LCELL(ABS(INCELL)))) THEN
 2001       CONTINUE
            CALL TRMOUT(80,' CELLS AVAILABLE : (0 FOR EXIT)^')
            DO 500 I = 1, 96
              IF (LCELL(I)) THEN
 9005           FORMAT(1X,I6,' (',F4.1,')')
                WRITE(6,9005) I,CL2HEU(I)
              ENDIF
 500        CONTINUE
            CALL TRMOUT(80,'>>>>^')
            INCELL = TERNUM(DUMM)
CX          CALL TRMIN(4,INPUT)
CX          CALL GETINT(INPUT,INCELL)

C                                   IF INPUT EQ 0 EXIT
            IF (INCELL.EQ.0) THEN
              RETURN
            ENDIF

C                                   CHECK FOR LEGAL CELL NUMBER
            IF (INCELL.LT.-96 .OR. INCELL.GT.96 ) THEN
              CALL TRMOUT(80,' ILLEGAL CELL NUMBER^')
              GOTO 2001
            ENDIF

          ENDIF

C                                   REQUEST FOR WIRE RANGE
          IF ( INCELL.LT.0) THEN
 2002       CONTINUE
            WRITE(6,9020)
 9020       FORMAT(' GIVE WIRE RANGE 1...16 (MIN,MAX) :')
            READ (5,*) NWIRNG
CX          WRITE (6,9031) NWIRNG
 9031       FORMAT(' WIRE RANGE :',2I4)
            INCELL = -INCELL
            IF (NWIRNG(2).LT.NWIRNG(1)) THEN
              NSAVE = NWIRNG(1)
              NWIRNG(1) = NWIRNG(2)
              NWIRNG(2) = NSAVE
            ENDIF
            IF (NWIRNG(1).LT.1 .OR. NWIRNG(2).GT.16) GOTO 2002
          ELSE
            NWIRNG(1) = 1
            NWIRNG(2) = 16
          ENDIF

          ICELL = INCELL
          IF (NPCELL(ICELL).LE.0) THEN
            CALL TRMOUT(80,' NO DATA AVAILABLE FOR THIS CELL^')
            RETURN
          ENDIF
C                                   FIND POINTERS FOR JETC DATA
          DO 4004 NW = 1, 16
            LWIRE(NW) = .FALSE.
            NP0WIR(NW) = 0
            ISTWIR(NW) = 0
            IENWIR(NW) = 0
 4004     CONTINUE
          IPJETC = IW(IBLN('JETC'))
          IF (IPJETC.GT.0) THEN
            IPJET2 = IW(IPJETC - 1)
            IF (IPJET2.GT.0) THEN
              IPJETC = IPJET2
            ENDIF
            ICLPNT = ( IPJETC + 1 ) * 2
CXXX        NP0WIR = ICLPNT + 98
            NCELWD = HW(ICLPNT + ICELL + 1) - HW(ICLPNT + ICELL)
CX          WRITE (6,9100) ICLPNT, NCELWD
 9100       FORMAT(' ICLPNT,NCELWD :',2I8)
            IF (NCELWD.GT.0) THEN
              ISTCEL = HW(ICLPNT + ICELL) + ICLPNT + 98
              IENDCL = ISTCEL + NCELWD
              IPHIT = ISTCEL
 4000         IF (IPHIT.GE.IENDCL) GOTO 4001
                IWADDR = HW(IPHIT)
                IWJETC = IWADDR / 8
                IWREL = IWJETC - (ICELL - 1) * 16 + 1
                IF (IWREL.LT.1 .OR. IWREL.GT.16) THEN
                  CALL TRMOUT(80,' ERROR IN J68K FORMAT^')
                  RETURN
                ENDIF
                IF (.NOT.LWIRE(IWREL)) THEN
                  LWIRE(IWREL) = .TRUE.
                  NP0WIR(IWREL) = ICLPNT + 98
                  ISTWIR(IWREL) = IPHIT
                  IENWIR(IWREL) = IPHIT + 4
CX                WRITE (6,9103) IWREL, IPHIT
 9103             FORMAT(' IWREL,IPHIT :',2I8)
                ELSE
CX                WRITE (6,9102) IWREL, IPHIT
 9102             FORMAT(' END OF IWREL,IPHIT :',2I8)
                  IENWIR(IWREL) = IPHIT + 4
                ENDIF
                IPHIT = IPHIT + 4
                GOTO 4000
 4001         CONTINUE
            ELSE
              CALL TRMOUT(80,' NO HITS ACCEPTED^')
            ENDIF
          ELSE
            CALL TRMOUT(80,' NO JETC BANK') ! PMF 19/10/99: Add `80' to parameter list
          ENDIF
C
C                                   RESET GRAPHIC TERMINAL
C
          CALL ERASE
CXXX      CALL TWINDO(0,4095,0,4095)
          CALL DWINDO(0.,4095.,0.,4095.)
C
C                                   SET LEFT/RIGHT SEPARATION FLAG
          LSEP = DSPDTM(20)

C                                   SET DOS FLAG
          LDOS = DSPDTM(21)

C                                   SET LINEAR/NONLINEAR FLAG
          LINEAR = DSPDTM(22)
C                                   SET FLAG FOR NORMPULS DIPLAY
          LAMP = .TRUE.

C                                   FIND CRATE FROM CELL NUMBER
          IDL300 = CL2PRC(ICELL - 1)
CX        WRITE(6,9008) IDL300
 9008     FORMAT(' CRATE NUMBER FROM CELL :',I6)

          NPSCAN = NPCELL(ICELL)
C                                   END OF SECTION FOR CELL
          L2 = NENDCL(ICELL)
CX        WRITE(6,9009) L2
 9009     FORMAT(' END OF SECTION FOR CELL :',I6)

C                                   RESET PEDESTALS
          DO 3001 NW = 1, 16
             IPED(1,NW) = 0
             IPED(2,NW) = 0
 3001     CONTINUE
C                                   FIND PEDESTALS FOR CELL
          DO 3000 NW = NWIRNG(1), NWIRNG(2)
 2700       IF (NPSCAN.GE.L2) GOTO 2900
              L3 = HW(NPSCAN+1)
              IF (L3.GT.5) THEN
                IWIRE = HW(NPSCAN+2)/256
                ICWIR = IWIRE - (ICELL - PRC2CL(IDL300) - 1)*16 + 1
                IF (NW .EQ. ICWIR) THEN
                  IPED(1,NW) = HW(NPSCAN+3)
                  IPED(2,NW) = HW(NPSCAN+4)
                ENDIF
CX              WRITE (6,9032) NW,IPED(1,NW),IPED(2,NW)
 9032           FORMAT(' NW,IPED :',3I5)
              ENDIF
              NPSCAN = NPSCAN + L3
              IF (NW.GE.ICWIR) GOTO 2700
 2900       CONTINUE
            NPSCAN = NPSCAN - L3
 3000     CONTINUE

          NPSCAN = NPCELL(ICELL)
C                                   END OF SECTION FOR CELL
          L2 = NENDCL(ICELL)
C                                   DRAW KOORDINATE AXES
          CALL FADCAX(ICELL,LINEAR,LDOS,LSEP)
CX        CALL TRMOUT(80,' RETURN FROM FADCAX^')
          DO 1000  NW = NWIRNG(1), NWIRNG(2)

            DO 300 I = 1,2
              DO 400 J = 1, 300
                IF (.NOT.LINEAR) THEN
                  IAMP(J,I) = LN2RAW(REAL(IPED(I,NW)))
                ELSE
                  IAMP(J,I) = IPED(I,NW)
                ENDIF
  400         CONTINUE
  300       CONTINUE
CX          CALL TRMOUT(80,' RESET OF IAMP^')
            LWIFND = .FALSE.
CX          WRITE(6,9010) NPSCAN
 9010       FORMAT(' POINTER FOR DATA :',I6)
 1700       IF( NPSCAN .GE. L2 ) GO TO 1900

C                                   NUMBER OF WORDS FOR PULS TRAIN
              L3 =  HW(NPSCAN+1)
CX            WRITE(6,9011) L3
 9011         FORMAT(' NUMBER OF WORDS FOR PULS :',I6)
              IF( L3.GT.5 ) THEN
                IWIRE = HW(NPSCAN+2)/256
                ICWIR = IWIRE - (ICELL - PRC2CL(IDL300) - 1)*16 + 1
CX              WRITE (6,9014) ICWIR,IWIRE,NW
 9014           FORMAT(' ICWIR,IWIRE,NW :',I4,I4,I4)
                IF (NW .EQ. ICWIR) THEN
 9006             FORMAT(' WIRE FOUND :',I6)
CX                WRITE(6,9006) NW
                  ITOFF = HW(NPSCAN+2) - IWIRE*256
CX                WRITE (6,9012) ITOFF
 9012             FORMAT(' STARTBIN OF PULS :',I6)
                  IAFAC = HW(NPSCAN+5)
                  NPSCAN = NPSCAN + 5
                  L3 = L3 - 5
 9007             FORMAT(' NUMBER OF TIMEBINS :',I6)
CX                WRITE(6,9007) L3
                  IF( L3 .LE. 300 ) THEN
                    DO 800 IBIN=1,L3
                      IA = HW(NPSCAN+IBIN)

                      IF (.NOT. LINEAR ) THEN
C                                              GET LEFT AND RIGHT AMP.
                        IAMP(ITOFF+IBIN,1) = IA/256
                        IAMP(ITOFF+IBIN,2) = LAND(IA,255)

                      ELSE
C                                              GET LINEAR AMP
                        IAMP(ITOFF+IBIN,1) = RAW2LN(IA/256)
                        IA = LAND(IA,255)
                        IAMP(ITOFF+IBIN,2) = RAW2LN(IA)
                      ENDIF

  800               CONTINUE
CC                  IF( IPRINT .LT. IPRLIM ) THEN
CC                    CALL FABPLT( ITOFF, L3, HW(NPSCAN+1) )
CC                  ENDIF
CC                  IF( IPRINT .LT.   0 .AND. L3.LT.11 ) THEN
CC                    CALL FABPLT( ITOFF, L3, HW(NPSCAN+1) )
CCC                 ENDIF

                    LWIFND = .TRUE.
                  ENDIF
                ENDIF
              ENDIF
              NPSCAN = NPSCAN + L3
CX            CALL TRMOUT(80,' NEXT^')
CX            WRITE(6,9010) NPSCAN
              IF (NW.GE.ICWIR) GO TO 1700
 1900       CONTINUE
            NPSCAN = NPSCAN - L3
C                                   DRAW FADC DATA
C           IF (LWIFND) THEN
              IWR = NW
CX            CALL TRMOUT(80,' CALL FADDRW^')
              CALL FADDRW(LDOS,LSEP,LINEAR)
CX            CALL TRMOUT(80,' RETURN FROM FADDRW^')
C           ENDIF
C                                   DRAW NORMPULS
C           IF (LAMP) THEN
C             CALL FADNOR(LDOS,LSEP,LINEAR,AMP)
C           ENDIF
 1000     CONTINUE
        ELSE
          CALL TRMOUT(80,' NO SCAN_DATA AVAILABLE^')
        ENDIF
      ELSE
        CALL TRMOUT(80,' NO J68K_DATA AVAILABLE^')

      ENDIF
C                                   SET SCREEN WINDOW BACK TO LAST VIEW
      CALL SETSCL(JTVW)
      RETURN
      END
