C   10/10/84 807241744  MEMBER NAME  DRVCRD   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DRVCRD( INDEX, IPBPCH, ILBPCH )
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. HAGEMANN   17/10/84 :  MAIN PROGRAM FOR DRAWING
C                                        VERTEX CHAMBER RAW-DATA
C                                        FROM  B P C H - BANK
C
C       MOD:   J. HAGEMANN   24/10/84 :  FOR FORMAT CHANGE
C       MOD:   J. HAGEMANN   22/04/85 :  CHANGES DUE TO NEW MICRO-
C                                        PROCESSOR ONLINE PROGRAM
C       MOD:   J. HAGEMANN   07/10/85 :  CHANGES DUE TO NEW MICRO-
C                                        PROCESSOR ONLINE PROGRAM
C  LAST MOD:   J. HAGEMANN   00/06/86 :  IMPROVED PRINTOUT
C
C     THE COORDINATES OF HITS ARE DRAWN FROM THE VERTEX CHAMBER RAW
C     DATA ( BPCH - BANK ). THE HITS FROM DIFFERENT WIRE ENDS
C     ( +Z, -Z PART OF CHAMBER) ARE DRAWN WITH CROSSES AND CIRCLES.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      INTEGER*4 CONVRT
C
      LOGICAL TBIT, FL18, FL22, FL24
C
#include "cgraph.for"
#include "cdata.for"
#include "cjvcel.for"
#include "cjvtxc.for"
C
      COMMON/CWORK1/R,FI,R1,FI1,X1,Y1,R2,FI2,X2,Y2,ZET,EX,EY,COSPH,SINPH
     +             ,KZAMP
C
      COMMON / CJTRIG / PI,TWOPI
      COMMON / CPROJ  / XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
      COMMON / CHEADR / HEAD(108)
C
      DIMENSION IHA(150), HBYTE(150)
C
C-----------------  C O D E  -------------------------------------------
C
C                            POINTER OF BANK (I*2)
      IPHVCR = IPBPCH*2
C                            LENGTH OF BANK  (I*2)
      ILHVCR = ILBPCH*2
C                            SET ACTUAL POINTER IN BANK
      IAPOIN = IPHVCR + 2
C
      ILSUMB = 0
C
C                  MAIN LOOP OVER EVERY MICROPROCESSOR-STRING
C
C                            GET LENGTH OF MICROPROCESSOR-STRING (I*2)
 1000 ILMPST = HDATA(IAPOIN + 1)/2
C                            GET VERSION NUMBER OF MICROPROCESSOR PRGM.
      IVERS  = HDATA(IAPOIN + 6)/1024
C                            CHECK IF MP-STRING IS CORRECT
         IF( HDATA(IAPOIN + ILMPST) .NE. -2 ) GO TO 2000
             GO TO 4000
 2000    IF( HDATA(IAPOIN + ILMPST) .NE. -1  .OR.
     &       HDATA(IAPOIN + ILMPST - 1) .NE. -2 ) GO TO 9000
C
C                            PROCESS MICROPROCESSOR-STRING
C                            ERRORWORD AND NUMBER OF HITS IN MP-STRING
 4000    IPHELP = IAPOIN + 1
         IERR  = LAND(HDATA( IPHELP+2 ),32767)
         NHITS = HDATA( IPHELP+3 )
C                            CHECK IF GOOD DATA
         IF( IERR .NE. 0 .OR. NHITS .EQ. 0 ) GO TO 5000
C                            SET ACTUAL POINTER
            IPMPST = IPHELP + 5
C
            ILSUMM = 0
C
C                  MAIN LOOP TO PROCESS WIRESIDE-STRING
C                            GET WIRESIDE-STRING LENGTH
 4100       ILWSID = HDATA( IPMPST+1 )/2
C                  CHECK IF WIRESIDE STRING IS CORRECT
            IF( HDATA( IPMPST + ILWSID) .NE. -8 ) GO TO 4900
C                            PROCESS WIRESIDE-STRING
C                            SET ACTUAL POINTER
               IPWSID = IPMPST + 2
C                            GET WIRESIDE NUMBER AND WIRE NUMBER
               NWSID  = HDATA( IPWSID )
               NWIR   = (CONVRT(NWSID) + 1)/2
C                            CHECK IF LEGAL WIRE NUMBER
               IF( NWIR .GT. 0 .AND. NWIR .LT. 169 ) GO TO 4150
                  CALL DIAGIN(' ILLEGAL WIRE NUMBER DETECTED',1,NWIR)
                  GO TO 4800
C                            GET CELL NUMBER AND WIRE NUMBER WITHIN CELL
 4150          NCEL  = (NWIR - 1)/MWIRE
               NWRC  = NWIR - NCEL*MWIRE
C
C                            IF FLAG SET FOR EVEN WIRE : CONTINUE
               IF( DSPDTL(26) .AND. TBIT(NWRC,31) ) GO TO 4800
               IF( FL18 .AND. FL22 )  GO TO 4800
C
C                            SET GEOMETRY VALUES
               R     = DISTW1 + DISTPW*FLOAT(NWRC-1)
               FI    = ANG1 + ANG2*FLOAT(NCEL)
               IF( FI .LT. 0 )     FI = FI + TWOPI
               IF( FI .GT. TWOPI ) FI = FI - TWOPI
               COSPH  = COS(FI)
               SINPH  = SIN(FI)
               XWIRE  = R*COSPH
               YWIRE  = R*SINPH
               EEX    = -SINPH*CSLORA(NCEL+1) - COSPH*SNLORA(NCEV+1)
               EEY    =  COSPH*CSLORA(NCEL+1) - SINPH*SNLORA(NCEV+1)
C
C
               ISIDE  = LAND(NWSID,1)
C
               ILSUMH = 0
C
               DO 4170 I = 1, 150
                  HBYTE(I) = 0
 4170          CONTINUE
C
               IEND   = ILWSID - 3
C                            CONVERT BYTES IN WIRESIDE-STRING INTO I*2
               DO 4180 I = 1, IEND
                  IHB = I*2 - 1
                  ILB = I*2
C
                  HBYTE(IHB) = LAND( HDATA( IPWSID + I ), 65280 )/256
                  HBYTE(ILB) = LAND( HDATA( IPWSID + I ), 255 )
C
 4180          CONTINUE
C                  MAIN LOOP TO PROCESS HIT
C
               IPHELP = 0
C
 4200          DO 4300 I = 1, 75
C
                  IHA(I) = HBYTE( IPHELP + I )
C
                  IF( IHA(I) .LT. 248 ) GO TO 4300
                     ILHIT = I
                     IENDO = I
                     GO TO 4400
 4300          CONTINUE
C
 4400          IF( HEAD(18) .GT. 18778 ) GO TO 4420
                  NCHA   = IHA( 1 )/2
                  IPED   = IHA( 2 ) - 2
                  IWIDTH = IHA( 3 )
                  GO TO 4425
 4420          NCHA   = IHA( 2 )/2
               IPED   = IHA( 3 ) - 2
               IF( HEAD(18) .GT. 19121 ) IPED = (IHA(3) + 2)/4
               IWIDTH = IHA( 1 ) - 4
 4425          IF( IWIDTH .LT. 2 ) GO TO 4600
                  ICHA1  = IHA( 4 )
                  ICHA2  = IHA( 5 )
                  IF( IVERS .LT. 5 ) GO TO 4430
                     ICHA1  = IHA( 5 )
                     ICHA2  = IHA( 6 )
C
C                            CHECK IF SUM OF CHANNELS > 0
 4430             IF( ICHA1 + ICHA2 - 2*IPED .LT. 1 ) GO TO 4450
C                            CHECK IF ONE OF THE 2 CHANNELS
C                            IS AN ENDMARK
                  IF( ICHA1 .GT. 247 .OR. ICHA2 .GT. 247 ) GO TO 4600
C                            GET DRIFT TIME
                  DRFTME = DRFTIM( NCHA, ICHA1, ICHA2, IPED, 0 )
C                            GET DRIFT WAY
                  DRFWAY = DRFTME*DRVELO( NCEL+1 )
C
C                            CALCULATE COORDINATES OF DRIFT TIME IN
C                            R-FI-VIEW
C
                  CALL CALCOR( XWIRE, YWIRE, EEX, EEY, DRFWAY )
C
                  IF( ISIDE .EQ. 0 ) GO TO 4440
                     CALL HITMRK( INDEX, -X1, Y1, 1.0, IDC )
                     CALL HITMRK( INDEX, -X2, Y2, 1.0, IDC )
                     GO TO 4450
 4440             CALL PLYGON( 9, 1.25, -X1, Y1, 1 )
                  CALL PLYGON( 9, 1.25, -X2, Y2, 1 )

C
C                            BRANCH FOR DOUBLE HITS
 4450             NCD = 2
                  IST = 3
                  IF( IVERS .GT. 4 ) IST = 4
C
                  DO 4500 I = 3, IENDO
                     IF( IHA(I+3) .GT. 247 ) GO TO 4600
C
                     IF( IHA(I+3) .NE. 240 ) GO TO 4480
                     IF( I+4 .GT. IENDO .OR. I+5 .GT. IENDO ) GO TO 4600
                     IF( IHA(I+4) .GT. 247 .OR. IHA(I+5) .GT. 247 )
     &                                                      GO TO 4600
                     IF( IHA(I+4) + IHA(I+5) - 2*IPED .LT. 1 )
     &                                                      GO TO 4480
                        DRFTME = DRFTIM( NCHA, IHA(I+4), IHA(I+5),
     &                                   IPED, NCD )
                        DRFWAY = DRFTME*DRVELO( NCEL+1 )
C
                        CALL CALCOR( XWIRE, YWIRE, EEX, EEY, DRFWAY )
C
                        IF( ISIDE .EQ. 0 ) GO TO 4460
                           CALL HITMRK( INDEX, -X1, Y1, 1.0, IDC )
                           CALL HITMRK( INDEX, -X2, Y2, 1.0, IDC )
                           GO TO 4470
 4460                   CALL PLYGON( 9, 1.25, -X1, Y1, 1 )
                        CALL PLYGON( 9, 1.25, -X2, Y2, 1 )
 4470                   GO TO 4500
 4480                NCD = NCD + 1
 4500             CONTINUE
C
 4600             ILSUMH = ILSUMH + ILHIT
C
                  IF( ILSUMH .GT. (ILWSID*2 - 6) ) GO TO 4800
C
                  IF( ILSUMH .EQ. (ILWSID*2 - 6) .OR.
     &                ILSUMH .EQ. (ILWSID*2 - 7) )  GO TO 4800
                     IPHELP = IPHELP + ILHIT
                     GO TO 4200
 4800 CONTINUE
C
C                            CHECK IF LAST WS-STRING
C
            ILSUMM = ILSUMM + ILWSID
C                            SAFETY CHECK IF END OF MP-STRING
            IF( ILSUMM .GT. (ILMPST-7) ) GO TO 5000
C                            LAST WIRESIDE-STRING ?
            IF( ILSUMM .EQ. (ILMPST-7) .OR. ILSUMM .EQ. (ILMPST-8) )
     &                                         GO TO 5000
               IPMPST = IPMPST + ILWSID
               GO TO 4100
 4900    CALL TRMOUT(80,' DRVCRD: ERROR IN BPCH WIRESIDE-STRING^')
 5000 CONTINUE
C
C                            CHECK IF LAST MP-STRING
C
            ILSUMB = ILSUMB + ILMPST
C                            SAFETY CHECK IF END OF BANK
            IF( ILSUMB .GT. ILHVCR-2 ) GO TO 9999
C                            LAST MP-STRING ?
            IF( ILSUMB .EQ. ILHVCR-2 .OR. ILSUMB .EQ. ILHVCR-3 )
     &                                               GO TO 9999
               IAPOIN = IAPOIN + ILMPST
               GO TO 1000
 9000    CALL TRMOUT(80,' DRVCRD: ERROR IN BPCH MICROPROCESSOR-STRING^')
 9999 CONTINUE
      RETURN
      END
