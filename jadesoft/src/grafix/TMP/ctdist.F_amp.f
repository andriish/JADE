C   09/03/84 807241310  MEMBER NAME  CTDIST   (S)        M  FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE CTDIST
C-----------------------------------------------------------------------
C
C   AUTHOR:  J. HAGEMANN   19/09/86 : CALCULATES DISTANCE BETWEEN TWO
C                                     POINTS GIVEN BY JOYSSTICK INPUT
C                                     (MODE 0)
C  LAST MOD: J. HAGEMANN   24/11/86 : CALCULATE DISTANCE BETWEEN SELEC-
C                                     TED TRACK AND ONE POINT GIVEN BY
C                                     JOYSSTICK (MODE 1)
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL FLVCDO
C
#include "cdata.for"
#include "cgraph.for"
C
      COMMON / CWORK1 / HWORK(40),SIZEWN,RANGE,XCENT,YCENT
      COMMON / CVX    / NNPATR,ICRSTR,NNJETC,NNVTXC
      COMMON / CGVCDO / FLVCDO(20)
C
C------------------  C O D E  ------------------------------------------
C
C                            CHECK IF DIST IS POSSIBLE FOR THIS VIEW
      IF( LASTVW .GT. 11 .AND. LASTVW .LT. 17 ) GO TO 4000
C                            GET MODE GIVEN BY FIRST ARGUMENT
      MODE = IFIX(ACMD)
      IF( MODE .EQ. 0 .OR. MODE .EQ. 2 ) GO TO 400
C
C                            SELECT TRACK NUMBER AND POINT ON SCREEN
C
C                            CHECK IF DIST IS POSSIBLE FOR THIS VIEW
      IF( LASTVW .GT. 3 .AND. LASTVW .NE. 17 .AND. LASTVW .NE. 20 )
     *         GO TO 4000
      CALL CLOC(IPPATR,'PATR',NNPATR)
      IF( IPPATR .GT. 0 ) GO TO 310
         WRITE(6,9301) NNPATR
 9301    FORMAT(' PATR BANK NR',I3,' (GIVEN BY COMMAND PATR) NOT EXISTIN
     *G')
         GO TO 5000
  310 CONTINUE
      IPHWDS = IDATA(IBLN('HWDS'))
      IF( FLVCDO(15) .AND. IPHWDS .GT. 0 ) IPPATR = IPHWDS
      IHD = IDATA(IPPATR+1)
      ITL = IDATA(IPPATR+3)
      NTR = IDATA(IPPATR+2)
      CALL TRMOUT(80,' Select track number:^')
  320 CALL TRMIN( 4, ICD )
      CALL GETINT( ICD, ITRK )
      IF( ITRK .GT. 0 .AND. ITRK .LE. NTR ) GO TO 330
C
         WRITE(6,9321) ITRK,NTR
 9321    FORMAT(' Track number ',I3,' is outside valid range (1 - ',I3,'
     *)')
         GO TO 320
C
  330 CALL TRMOUT(80,' Now select point with hair cross!^')
C
      CALL VCURSR(HWORK(1),XGET,YGET)
C
      IPTR = IPPATR + IHD + (ITRK-1)*ITL
      CALL DRTRCK(IPTR,-XGET,YGET,DR)
      DR = ABS(DR)
      WRITE(6,9331) DR
 9331 FORMAT(' DISTANCE: ',F12.3)
C
      GO TO 5000
C
C                            FLASH CURSOR ON SCREEN AND AWAIT RESULT
C
  400 CALL TRMOUT(80,' Now select FIRST point with hair cross!^')
C
      CALL VCURSR(HWORK(1),XGET1,YGET1)
C
      CALL TRMOUT(80,' Now select SECOND point with hair cross!^')
C
      CALL VCURSR(HWORK(1),XGET2,YGET2)
C
      DD12 = SQRT((XGET2-XGET1)**2 + (YGET2-YGET1)**2)
      WRITE(6,9401) DD12
 9401 FORMAT(' DISTANCE: ',F12.3)
C
      GO TO 5000
C
 4000 CALL TRMOUT(80,' Sorry, DIST not available for this view!^')
C
 5000 CONTINUE
C
      RETURN
      END
