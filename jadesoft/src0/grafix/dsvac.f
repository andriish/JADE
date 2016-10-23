C   10/10/84 807241745  MEMBER NAME  DSVAC    (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DSVAC
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. HAGEMANN   19/10/84 :  MAIN PROGRAM FOR DRAWING
C                                        SIGNAL AMPLITUDES FOR A
C                                        SELECTED CELL IN VERTEX CHAMBER
C
C       MOD:   J. HAGEMANN   22/04/85 :  CHANGES DUE TO NEW MICRO-
C                                        PROCESSOR ONLINE PROGRAM
C       MOD:   J. HAGEMANN   12/07/85 :  CODE SPEEDED UP, SUBROUTINE
C                                        GETINT NOW IN OWN MEMBER,
C                                        CHANGES DUE TO NEW MICRO-
C                                        PROCESSOR ONLINE PROGRAM AND
C                                        FOR DIFFERENCIATED PULSES
C  LAST MOD:   J. HAGEMANN   13/02/86 :  FOR BANK VCVW
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL LSEP, LDIF, LVIEW, FLVCDO
C
      INTEGER*4 CONVRT
C
#include "cgraph.for"
#include "cdata.for"
#include "cjvcel.for"
C
      COMMON / CGVCDO / FLVCDO(20)
      COMMON / CWORK1 / HAMP(2,60), HSTORE(2,420), ISCLOW, ISCHIG
      COMMON / CHEADR / HEAD(108)
      COMMON / CVX    / NNPATR,NDUMM,NNJETC,NNVTXC
C
      DIMENSION IHA(150), HBYTE(150)
C
C-----------------  C O D E  -------------------------------------------
C
C                            CHECK IF MONTE CARLO DATA
      IF( HEAD(18) .GT. 100 ) GO TO 5
         CALL TRMOUT(80,' VAC  not available for MONTE CARLO DATA^')
         RETURN
C                                      TEST IF RAW DATA BANK  B P C H
C                                      IS EXISTING
    5 IPBPCH = IDATA(IBLN('BPCH'))
      IF( IPBPCH .LE. 0 )  RETURN
C
      ISCLOW = 0
      ISCHIG = 64
      LVIEW  = .FALSE.
C
      IF( NNVTXC .LT. 0 .OR. NNVTXC .GT. 99 ) GO TO 50
         CALL CLOC( IPVCVW, 'VCVW', NNVTXC )
         LVIEW = .TRUE.
         IF( IPVCVW .GT. 0 ) GO TO 55
            LVIEW = .FALSE.
            IF( NNVTXC .NE. 10 )
     &      WRITE(6,40) NNVTXC
   40       FORMAT(' VCVW BANK NR',I3,' (GIVEN BY COMMAND VTXC) NOT EXIS
     &TING')
C
   50 ILBPCH = IDATA(IPBPCH)
C
      IF( ILBPCH .LE. 1 ) RETURN
C
   55 LDIF = .FALSE.
      NC   = ACMD
      IF( NC .GT. 0 ) GO TO 65
   60    CALL TRMOUT(80,' Enter Cell Number^')
         CALL TRMIN( 4, ICD )
         CALL GETINT( ICD, NC )
   65 IF( NC .LT. 100 ) GO TO 70
          NC = NC - 100
          LDIF = .TRUE.
   70 IF( NC .GT. 0 .AND. NC .LT. 25 ) GO TO 80
         CALL TRMOUT(80,' Illegal Cell Number^')
         GO TO 60
   80 CALL ERASE
      CALL TWINDO(0,4095,0,4095)
C
      DO 95 I = 1, 2
         DO 90 J = 1, 420
            HSTORE( I, J ) = 0
   90    CONTINUE
   95 CONTINUE
C
      NWLOW  = NC*MWIRE - 6
      NWHIG  = NC*MWIRE
C                                      FOR VCVW BANK DISPLAY
      IF( LVIEW ) GO TO 2000
C
      IPHVCR = IPBPCH*2
      ILHVCR = ILBPCH*2
      IAPOIN = IPHVCR + 2
      ILSUMP = 0
C
  100 ILMPST = HDATA(IAPOIN + 1)/2
      IVERS  = HDATA(IAPOIN + 6)/1024
C                            CHECK IF MP-STRING IS CORRECT
         IF( HDATA(IAPOIN + ILMPST) .NE. -2 ) GO TO 200
            GO TO 400
  200    IF( HDATA(IAPOIN + ILMPST) .NE. -1  .OR.
     &       HDATA(IAPOIN + ILMPST - 1) .NE. -2 ) GO TO 900
C
C                            PROCESS MICROPROCESSOR-STRING
C
  400       IPMPST = IAPOIN + 6
            ILSUMW = 0
C
C                  MAIN LOOP TO PROCESS WIRESIDE-STRING
C                            GET WIRESIDE-STRING LENGTH
  500       ILWSID = HDATA( IPMPST+1 )/2
C                  CHECK IF WIRESIDE STRING IS CORRECT
            IF( HDATA( IPMPST + ILWSID) .NE. -8 ) GO TO 750
C                            PROCESS WIRESIDE-STRING
            IPWSID = IPMPST + 2
C                            GET WIRESIDE NUMBER AND WIRE NUMBER
            NWSID = HDATA( IPWSID )
            NWIR  = (CONVRT(NWSID) + 1)/2
C                            CHECK IF WIRE IS IN SELECTED CELL
            IF( NWIR .LT. NWLOW .OR. NWIR .GT. NWHIG ) GO TO 730
C                            GET CELL NUMBER AND WIRE NUMBER WITHIN CELL
            NCEL  = (NWIR - 1)/MWIRE
            NWRC  = NWIR - NCEL*MWIRE
C
            ISIDE  = LAND(NWSID,1) + 1
C
            ILSUMH = 0
C
            IPDSTA = 1
C
           DO 505 I = 1, 150
              HBYTE(I) = 0
  505      CONTINUE
C
            IEND   = ILWSID - 3
C                            CONVERT BYTES IN WIRESIDE-STRING INTO I*2
            DO 510 I = 1, IEND
               IHB = I*2 - 1
               ILB = I*2
C
               HBYTE(IHB) = LAND( HDATA( IPWSID + I ), 65280 )/256
               HBYTE(ILB) = LAND( HDATA( IPWSID + I ), 255 )
C
  510       CONTINUE
C
C                  MAIN LOOP TO PROCESS HIT
C
            IPHELP = 0
C
  550       DO 570 I = 1, 75
               IHA(I) = HBYTE( IPHELP + I )
C
               IF( IHA(I) .LT. 248 ) GO TO 570
                  ILHIT = I
                  IENDO = I
                  GO TO 580
C
  570       CONTINUE
C
C
  580          IF( HEAD(18) .GT. 18778 ) GO TO 581
                  NCHA   = IHA(1)/2
                  IPEDAL = IHA( 2 ) - 2
                  GO TO 582
  581          NCHA   = IHA(2)/2
               IPEDAL = IHA( 3 ) - 2
               IF( HEAD(18) .GT. 19121 ) IPEDAL = (IHA(3) + 2)/4
C
  582          IPDEND = NCHA
CCCCC          IF( IVERS .GT. 4 ) IPDEND = IPDEND - 1
C
               DO 585 I = IPDSTA, IPDEND
                  IPFILL = (NWRC - 1)*60 + I
                  HSTORE( ISIDE, IPFILL ) = 300 + IPEDAL
  585          CONTINUE
C
               IPHS = 0
CCCCC          IF( IVERS .GT. 4 ) IPHS = -1
C
               IBEGO = 4
               IF( IVERS .GT. 4 ) IBEGO = 5
C
               DO 590 I = IBEGO, IENDO
                  IF( IHA(I) .GT. 247 ) GO TO 600
                     IF( IHA(I) .EQ. 240 ) GO TO 590
                        IPHS   = IPHS + 1
                        IPFILL = (NWRC - 1)*60 + NCHA + IPHS
                        HSTORE( ISIDE, IPFILL ) = IHA(I)
  590          CONTINUE
C
  600          IPDSTA = NCHA + IPHS + 1
C
C
               ILSUMH  = ILSUMH + ILHIT
C
               IF( ILSUMH .GT. (ILWSID*2 - 6) ) GO TO 700
C
                  IF( ILSUMH .EQ. (ILWSID*2 - 6) .OR.
     &                ILSUMH .EQ. (ILWSID*2 - 7) ) GO TO 700
                     IPHELP = IPHELP + ILHIT
                     GO TO 550
C                            LOAD PEDESTAL AFTER LAST HIT
  700       CONTINUE
            DO 720 I = IPDSTA, 60
               IPFILL = (NWRC - 1)*60 + I
               HSTORE( ISIDE, IPFILL ) = 300 + IPEDAL
  720       CONTINUE
C                            CHECK IF LAST WS-STRING
C
  730       ILSUMW = ILSUMW + ILWSID
C                            SAFETY CHECK IF END OF MP-STRING
            IF( ILSUMW .GT. (ILMPST-7) ) GO TO 800
C                            LAST WIRESIDE-STRING ?
               IF( ILSUMW .EQ. (ILMPST-7) .OR. ILSUMW .EQ. (ILMPST-8) )
     &                                         GO TO 800
                 IPMPST = IPMPST + ILWSID
                 GO TO 500
  750       CALL TRMOUT(80,' ERROR in Wireside-String^')
C
C                            CHECK IF LAST MP-STRING
C
  800       ILSUMP = ILSUMP + ILMPST
C                            SAFETY CHECK IF END OF BANK
            IF( ILSUMP .GT. ILHVCR-2 ) GO TO 8000
C                            LAST MP-STRING ?
            IF( ILSUMP .EQ. ILHVCR-2 .OR. ILSUMP .EQ. ILHVCR-3 )
     &                                               GO TO 8000
               IAPOIN = IAPOIN + ILMPST
               GO TO 100
  900    CALL TRMOUT(80,' ERROR in Microprocessor-String^')
         GO TO 8000
C
C                                      FOR VCVW BANK DISPLAY
 2000 IPVCV2 = IPVCVW*2
      ILVCV2 = IDATA(IPVCVW)*2
      ISCLOW = HDATA(IPVCV2+4)
      ISCHIG = HDATA(IPVCV2+5)
C
      IPARRY = IPVCV2 + HDATA(IPVCV2+1)
C
 2100 ILENWS = HDATA(IPARRY+1)
      INUMWS = HDATA(IPARRY+2)
      IWIRE  = (CONVRT(INUMWS) + 1)/2
C                                      CHECK IF WIRE IS IN SELECTED CELL
      IF( IWIRE .LT. NWLOW .OR. IWIRE .GT. NWHIG ) GO TO 2500
C                                      GET CELL NUMBER AND WIRE NUMBER
C                                      WITHIN CELL
         NCEL  = (IWIRE - 1)/MWIRE
         NWRC  = IWIRE - NCEL*MWIRE
C
         ISIDE = LAND(INUMWS,1) + 1
C
         ILINF = ILENWS - 2
C
         ICAN  = 1
C
         DO 2300 I = 1, ILINF
            IPWSI = IPARRY + 2 + I
            IWRD  = HDATA(IPWSI)
            IF( IWRD .LT. 200) GO TO 2200
               IWRD1 = IWRD/200
               IREP  = IWRD1
               DO 2150 INA = 1, IREP
                  IADD   = ICAN-1 + INA
                  IF( IADD .LE. 60 ) GO TO 2140
                     WRITE(6,9100) INUMWS
 9100                FORMAT(' DSVAC : ERROR IN WS',I4,'  IN VCVW BANK')
                     GO TO 2500
 2140             IPFILL = (NWRC - 1)*60 + IADD
                  HSTORE( ISIDE, IPFILL ) = IWRD - IWRD1*200
 2150          CONTINUE
               GO TO 2250
 2200       IF( ICAN .LE. 60 ) GO TO 2230
               WRITE(6,9100) INUMWS
               GO TO 2500
 2230       IPFILL = (NWRC - 1)*60 + ICAN
            HSTORE( ISIDE, IPFILL ) = IWRD
            IREP  = 1
 2250       ICAN = ICAN + IREP
 2300    CONTINUE
C
 2500 IPARRY = IPARRY + ILENWS
      IF( HDATA(IPARRY+1) .LE. 0 ) GO TO 8000
         IF( IPARRY .LT. IPVCV2+ILVCV2 ) GO TO 2100
C
 8000 LSEP = FLVCDO(6)
C
 8100 CONTINUE
C
      CALL KOORD( LSEP, NC, LDIF, LVIEW )
      DO 8200 I = 1, MWIRE
         CALL DRWHIT( I, LSEP, LDIF, NWLOW-1 + I )
 8200 CONTINUE
C
 8300 CALL SETSCL(LASTVW)
C
      RETURN
      END
