C   14/05/84 510232119  MEMBER NAME  TAGCHK   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE TAGCHK(IPHTGM,IPHTGP)
C-----------------------------------------------------------------------
C
C   AUTHOR:      ?      PREHISTORY :  TAGGING SYSTEM COMPUTATIONS
C
C        MOD: J. OLSSON    6/10/83 :
C        MOD: A. FINCH    12/03/84 :  TO CONFORM WITH TAGGING CHANGES
C   LAST MOD: J. NYE      14/05/84 :  INITIALIZE CAPSUMS  TO ZERO
C   LAST MOD: J. OLSSON   23/10/85 :  MONTE CARLO PEDESTALS PROPER
C
C
C
C        COMPUTE ENERGY SUMS FOR FORWARD TAGGING LEADGLASS
C        IPHTGM,IPHTGP ARE MINUS AND PLUS Z CAPSUMS
C        SUBTRACT STANDARD PEDESTAL OF 500 COUNTS, APPLY STANDARD
C        CALIBRATION FACTOR OF 5 MEV / COUNT
C        AFTER RUN 2782, PEDESTAL IS 50 COUNTS, CALIBR. FACTOR 7.5
C
C        FOR 1981 - 82 LEADGLASS, USE CALIBRATION FROM A.FINCH
C        FOR 1983 - ..            USE CALIBRATION FROM FINCH....
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER * 2 (H)
C
#include "cdata.for"
#include "cwktag.for"
      COMMON / CHEADR / HEAD(108)
C
      DIMENSION NPBPED(3 ) ,CALTAG(3)
C
      DATA NPBPED /500,50,0/, CALTAG /5.,7.5,5.5/
C
C------------------ C O D E ------------------------------------------
C
C
C---                                     INITIALIZATION
C
C
      PPHM = 0.0
      PPHP = 0.0
      CALL TAGINT(*300)
      IPNTR = IDATA( IBLN( 'ATAG' ) )
      IF ( IPNTR .LT. 1 ) GOTO 300
      IF ( MARK .GT. 0 ) GOTO 1919
C
C
C----------------------------------------1979/80 APPARATUS ONLY---------
C
C
C ENTER HERE FOR 1979 - 80 VERSION OF THE TAGGING APPARATUS
C
      IRUN = 1
      IF ( HEAD(18) .GT. 2781 ) IRUN = 2
      IF ( IMC.EQ.1 ) IRUN = 3
      IADD = 2 * IPNTR + 6
      IFLUMI = IADD + HDATA(IADD - 1)
C
200   IADD = IADD + 1
      IF ( IADD .GE. IFLUMI ) GOTO 300
      NB = HDATA(IADD)
      IADD = IADD + 1
      IF ( NB .EQ. 0 ) GOTO 200
      IF ( NB .GT.  46 .AND. NB .LT.  49 ) GOTO 200
      IF ( NB .GT.  94 .AND. NB .LT.  97 ) GOTO 200
      IF ( NB .GT. 142 .AND. NB .LT. 145 ) GOTO 200
      IF ( NB .GT. 190 ) GOTO 300
      HPH = HDATA(IADD)
      HPH = HPH - NPBPED(IRUN)
      IF ( HPH .LT. 0 ) GOTO 200
      PPH = CALTAG(IRUN) * HPH
      IF ( NB .LE. 96 ) PPHM = PPHM + PPH
      IF ( NB .GT. 96 ) PPHP = PPHP + PPH
      GOTO 200
C
C
C----------------------------------------1981--> APPARATUS ONLY---------
C
C
C
C
C  ENTER HERE FOR 1981 - .. APPARATUS
C
1919  CONTINUE
      CALL TAGADC(0,*300)
      CALL TAGPED
      CALL TAGKAL(0)
      CALL TAGSUM(-1,PPHM,*299)
299   CALL TAGSUM(+1,PPHP,*300)
300   IPHTGM = PPHM
      IPHTGP = PPHP
      RETURN
      END
